{-
 - Redistribution and use in source and binary forms, with or without modification, are permitted
 - provided that the following conditions are met:
 - 1.   Redistributions of source code must retain the above copyright notice, this list of
 - conditions and the following disclaimer.
 - 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 - conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 - THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 - BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 - EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 - OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 - OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 - IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, CPP, FlexibleContexts #-}
{-# OPTIONS -msse2 #-}

-- A Binary Tree Search Program
--
-- It buids a binary tree in memory as a flat array, and searches values in it.

import System.Environment
import Control.Monad (when)
import Data.Int (Int32)
import Data.Bits (unsafeShiftL)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import System.IO.Unsafe
import qualified Data.Vector.Unboxed as U
import GHC.Float (int2Float, float2Int)

#ifdef CRITERION
import Criterion.Main
import Criterion.Config
#elif !defined(QUICKCHECK)
import Harness
#endif

#ifdef REPA
import Data.Array.Repa hiding ((++), map, zipWith, (!))
import qualified Data.Array.Repa as R
#endif

#ifdef QUICKCHECK
import Test.QuickCheck
#else
import System.Random.Mersenne
type StdGen = MTGen
getDefaultGen = newMTGen (Just $ fromIntegral 0)
#endif

-- Always use Int32 for Key regardless of platform
type Key = Int32

-- Fast power of 2
pow2 :: Int -> Int
pow2 n = 1 `unsafeShiftL` n

pow2' :: Int -> Key
pow2' = fromIntegral . pow2

trace x = unsafePerformIO (print x >> return False)

pagesize  = 1024 -- 4096 bytes
cacheline = 16   -- 64 bytes
simdwidth = 4    -- 32 bytes

level1 = 10      -- 2 ^ level1 = pagesize, but we use 8 here to be divisible by level2
level2 = 4       -- 2 ^ level2 = cacheline
level3 = 2       -- 2 ^ level3 = simdwidth


#ifdef REPA
type Tree   = R.Array U DIM1 Key
(!) x i = x `R.unsafeIndex`  (Z :. i)
#else
type Tree   = U.Vector Key
(!) x i     = x `U.unsafeIndex` i
#endif

{-
 - Build an index tree of depth n. For example:
 -
 -   buildTree 4 = [7,3,11,1,5,9,13,0,2,4,6,8,10,12,14]
 -
 - represents the following tree:
 -
 -        7
 -    3       11
 -  1   5   9    13
 - 0 2 4 6 8 10 12 14
 -}
buildTree :: Int -> U.Vector Key
buildTree n = build' 0 (pow2' (n - 2)) $ U.singleton (pow2' (n - 1) - 1)
  where
    build' l w s | l >= n    = U.empty
                 | otherwise =
      let t = U.generate (U.length s * 2) fn
          fn i = let j = i `div` 2
                     v = s `U.unsafeIndex` j
                 in if i == j * 2 then v - w else v + w
      in s U.++ build' (l + 1) (w `div` 2) t

{-
 - Search a flattened index tree to find a key k. For simplification, we
 - assume k is the same as the index number, rather than the actual key
 - at the index. The search terminates with either Found, or an exit edge.
 -}
{-# INLINE flatSearch #-}
flatSearch :: Int -> Tree -> Key -> Bool
flatSearch limit tree k = search' 0
  where
    search' offset
#ifdef DEBUG
     | trace ("flat", offset) = undefined
#endif
     | offset >= limit = False
     | otherwise       =
      let u = tree ! offset
{-
      in case compare k u of
           LT -> search' (offset * 2 + 1)
           GT -> search' (offset * 2 + 2)
           EQ -> True
-}
      in if k == u then True
           else let gt = fromEnum $ k > u
                in search' (offset * 2 + 1 + gt)


{-
 - Rearrange a flat tree into block structure for better locality. For example:
 -
 -   rearrange 2 (buildTree 4) = [[7,3,11],[1,0,2],[5,4,6],[9,8,10],[13,12,14]]
 -
 - represents the same tree above, but with 2-level triangles (or blocks) such
 - as [7,3,11], [1,0,2], [5,4,6], etc., arranged in clusters so that they can
 - be loaded together in a single page/cacheline/SIMD load.
 -
 - If the height of input tree is not divisible by the block size, the leaf
 - blocks will be have the size of the remainder.
 -}
rearrange :: Int -> U.Vector Key -> [U.Vector Key]
rearrange d arr = concat $ map mkLevel $ zipWith (\x y -> (x, y - x)) lvls (tail lvls)
  where
    h = log2 (U.length arr + 1)  -- height of tree
    lvls = [0, d .. (h - 1)] ++ [h]
    leftMost l = pow2 l - 1
    mkLevel (y, d) = map (mkTree d y) [0..(width - 1)]
      where
        base  = leftMost y
        width = pow2 y
    mkTree d l base = U.generate (pow2 d - 1) f
      where
        f i = arr `U.unsafeIndex` (base * pow2 y + leftMost (y + l) + x)
          where y = log2 (i + 1)
                x = i - (pow2 y - 1)

-- Fast log2 for Int
log2 :: Int -> Int
log2 x = float2Int (log (int2Float x) / log 2)
{-
twoPows = U.generate 30 pow2
log2 x = fromJust (U.findIndex (>x) twoPows) - 1
-}

-- Build two level blocking tree. We fix the page level blocking size from
-- 1023 elements to 1024 in order to match the C algorithm (presumably for
-- better alignment). Consequently, search algorithms must be ajusted too.
buildBlockTree = U.concat . concat . alignHack .  map (rearrange level2) .
		 rearrange level1 . buildTree

alignHack = map (\l -> if (length l == 273) then l ++ [U.singleton (-1)] else l)

{-
 - An alternative to flatSearch that avoids early loop exit is to compute
 - the final exit index, which can be used as an index into the original
 - sorted key array to retrieve and compare whether the searched key is
 - found or not.
 -
 - Composing small traversing steps together achieves the equivalent
 - effect of loop unrolling, but this requires the depth to be fixed.
 -}
type IndexSearch = Int -> Tree -> Key -> Int

-- One step traversal down the tree
{-# INLINE stepSearch #-}
stepSearch :: Int -> Tree -> Key -> Int -> Int
stepSearch base tree key offset =
      let u = tree ! (base + offset)
#ifdef __PPILER__
-- the NOINLINE below is to prevent GHC from inlining computation down
-- braches so that we get only one cmov out of each comparison when
-- compiled by HRC. It doesn't seem to affect performance though except
-- that the result C code is cleaner to look at.
          {-# NOINLINE b #-}
#endif
          b = fromEnum (key > u)
      in offset * 2 + 1 + b

-- A general indexSearch of arbitrary depth
indexSearch d tree key = apply d (stepSearch 0 tree key) 0 - pow2 d + 1
  where
    apply 0 f x = x
    apply i f x = f (apply (i - 1) f x)

-- search tree of depth 2 and 4
blockSearch2, blockSearch4, blockSearch6, blockSearch10, blockSearch16, blockSearch24 :: IndexSearch

{-# INLINE blockSearch2 #-}
blockSearch2 base tree key = f (f 0) - 3
  where f = stepSearch base tree key

{-# INLINE blockSearch4 #-}
blockSearch4 base tree key = f (f (f (f 0))) - 15
  where f = stepSearch base tree key

{-
 - To search a key in a tree of nested blocks, one must handle
 - the search path differently.
 -
 - Note that the INLINE pragma is not absolutely necessary, but
 - without which HRC seems produce two aliases for the same input
 - array, which again doesn't affect performance but is unpleasant
 - to look at.
 -}

-- Block search of depth 6 comprises of one block of 6, and one block of 2.
{-# INLINE blockSearch6 #-}
blockSearch6 base tree k = f2 (blockSearch4 base tree k)
  where
    f2 offset =
      let x = (base + (b4 - 1) + (b2 - 1) * offset)
          l = blockSearch2 x tree k
      in offset * b2 + l
    b4 = pow2 4
    b2 = pow2 2

-- Block search of depth 10 comprises of two blocks of 4, and one block of 2.
{-# INLINE blockSearch10 #-}
blockSearch10 base tree k = f2 (f4 (f4 0) - b4 - 1)
  where
    f4 offset =
      let x = (base + (b4 - 1) * offset)
          l = blockSearch4 x tree k
      in offset * b4 + l + 1
    f2 offset =
      let x = (base + (b8 - 1) + (b2 - 1) * offset)
          l = blockSearch2 x tree k
      in offset * b2 + l
    b8 = pow2 8
    b4 = pow2 4
    b2 = pow2 2

-- Block search of depth 16 comprises of one block of 10, and one block of 6.
{-# INLINE blockSearch16 #-}
blockSearch16 base tree k = f6 (blockSearch10 base tree k)
  where
    f6 offset =
      let x = (base + b10 + (b6 - 1) * offset)
          l = blockSearch6 x tree k
      in offset * b6 + l
    b10 = pow2 10
    b6  = pow2 6

-- Block search of depth 24 comprises of two blocks of 10, and one block of 4.
{-# INLINE blockSearch24 #-}
blockSearch24 base tree k = f4 (f10 (f10 0) - b10 - 1)
  where
    f10 offset =
      let x = base + b10 * offset
          l = blockSearch10 x tree k
      in offset * b10 + l + 1
    f4 offset =
      let x = (base + b20 + b10 + (b4 - 1) * offset)
          l = blockSearch4 x tree k
      in offset * b4 + l
    b20 = pow2 20
    b10 = pow2 10
    b4  = pow2 4

searchDepth6  = blockSearch6  0
searchDepth10 = blockSearch10 0
searchDepth16 = blockSearch16 0
searchDepth24 = blockSearch24 0

#ifdef QUICKCHECK
-- Use QuickCheck to ensure that the above is implemented correctly.

data Depth = Depth Int deriving (Eq, Show)

instance Arbitrary Depth where
  arbitrary = fmap Depth $ choose (1, 10::Int)

propFlatSearch (Depth d)
  = all (flatSearch limit t) [0..fromIntegral (limit-1)]
  where limit = pow2 d - 1
        t = buildTree d

propIndexSearch (Depth d)
  = all (\i -> fromIntegral i == indexSearch d t i) [0..fromIntegral (limit-1)]
  where limit = pow2 d - 1
        t = buildTree d

data Depth' = Depth' Int deriving (Eq, Show)

instance Arbitrary Depth' where
  arbitrary = fmap Depth' $ elements [6, 10, 16, 24]

propBlockSearch (Depth' d) =
  forAll (vectorOf 1000 (choose (0, pow2' d - 2))) $ all (\ i -> search tree i == fromIntegral i)
  where (tree, search) = fromJust $ lookup d $ zip depths $ zip trees searches
        searches = [ searchDepth6, searchDepth10, searchDepth16, searchDepth24 ]
        trees = map buildBlockTree depths
        depths = [6, 10, 16, 24]

main = do
  quickCheck propFlatSearch
  quickCheck propIndexSearch
  quickCheck propBlockSearch

#else

#ifdef REPA
{-# INLINE compute #-}
compute f = R.computeUnboxedP . R.map f
mkArray size = R.fromUnboxed (Z :. size)
checksum = R.sumAllS
#else
compute f = return . U.map f
mkArray size = id
checksum = U.sum
#endif

flatQuery depth tree queries = limit `seq` compute fn queries
  where
    fn = fromEnum . flatSearch limit tree
    limit = pow2 depth - 1
    -- limit = float2Int (2 ** fromIntegral depth) - 1

{-# INLINE blockQuery #-}
blockQuery search tree queries = compute fn queries
  where
    fn i = fromEnum $ fromIntegral i == search tree i

mkQueries numQueries max = do
  g <- getDefaultGen
  U.replicateM numQueries $ fmap value $ random g
  where value v = abs v `mod` fromIntegral max

#ifdef CRITERION
main = do
  let depth = 16
  numQueries <- getArgs >>= \s -> if length s == 0 then return 10000000 else readIO (last s)
  putStrLn $ "Running treesearch, depth = " ++ show depth ++ " queries = "++ show numQueries
  let size = pow2 depth - 1
      flat = mkArray size $ buildTree depth
      nest = mkArray size $ buildBlockTree depth
  queries <- fmap (mkArray numQueries) $ mkQueries numQueries (pow2' depth - 1)
  flat `seq` nest `seq` defaultMainWith defaultConfig (return ())
    [ bgroup (show numQueries)
      [ bench "flatSearch"  $ whnfIO (flatQuery depth flat queries),
	bench "blockSearch" $ whnfIO (blockQuery searchDepth16 nest queries) ] ]
#else
buildIt args = do
  (t, depth, numQueries) <- case args of
         [t, d, q] -> return (t, read d, read q)
         _ -> error "Usage: treeSearch [flat|block] <tree depth> <number of queries>"
  let max = pow2 depth - 1
      size = max
  queries <- fmap (mkArray numQueries) $ mkQueries numQueries (fromIntegral max)
  if t == "flat"
    then do
      let arr = mkArray size $ buildTree depth
      arr `seq` return (flatQuery depth arr queries, Just (print . checksum))
    else do
      let arr = mkArray size $ buildBlockTree depth
          query = case depth of
                    6  -> blockQuery searchDepth6  arr queries
                    10 -> blockQuery searchDepth10 arr queries
                    16 -> blockQuery searchDepth16 arr queries
                    24 -> blockQuery searchDepth24 arr queries
                    _  -> error $ "The tree depth must be either 6, 10, 16 or 24 for block query"
      arr `seq` return (query, Just (print . checksum))
main = runBenchmark buildIt
#endif

#endif
