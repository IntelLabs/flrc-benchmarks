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
{-# LANGUAGE BangPatterns #-}

import Harness  
import Control.Monad ((>=>))
import Data.Array.Repa as R hiding ((!))
import qualified Data.Array.Repa as R
import Prelude hiding ((!!))

padding = 1
w1 = 1
w2 = 1

(!) :: Shape sh => Source r e => Array r sh e -> sh -> e
(!) = R.unsafeIndex

type CImage = Array U DIM3 Float

(!!) :: Source r e => Array r DIM3 e -> DIM3 -> e
(!!) img (Z:. z :. y :. x) = img ! (Z:. z + padding :. y + padding :. x + padding)

compute d h w img = R.computeP $ R.fromFunction (Z :. d :. h :. w) f
  where
    f sh@(Z:. z :. y :. x) = w1 * (img ! sh) + w2 * s
      where 
        s = img !! (Z :. z + 1 :. y :. x) +
            img !! (Z :. z :. y + 1 :. x) +
            img !! (Z :. z :. y :. x + 1) +
            img !! (Z :. z - 1 :. y :. x) +
            img !! (Z :. z :. y - 1 :. x) +
            img !! (Z :. z :. y :. x - 1)
    
initImage :: Int -> Int -> Int -> IO CImage
initImage d h w = R.computeP $ R.fromFunction (Z :. d + padding * 2 :. h + padding * 2 :. w + padding * 2) f
  where 
    inBound w x = x >= padding && (x < w + padding)
    inBoundZYX z y x = inBound d z && inBound h y && inBound w x
    f (Z :. z :. y :. x) | inBoundZYX z y x = fromIntegral $ 0 - 2 + x `mod` 2 + (y `mod` 2) * (z `mod` 2)
                         | otherwise        = 0

checksum :: CImage -> IO Double
checksum = R.sumAllP . R.map realToFrac

advance !n d h w img | n == 0    = return img
                     | otherwise = compute d h w img >>= advance (n - 1) d h w

buildIt args = do 
  [w, h, d, iter] <- case args of
             []           -> return $ [128, 384, 300, 1000]
             [_, _, _, _] -> mapM readIO args
             _            -> error "Usage: 7pt-stencil [<width> <height> <depth> <iterations>]\n"
  image <- initImage d h w
  let runIt  = advance iter d h w image
      showIt = Just $ checksum >=> writeFile "7pt-stencil.res" . show 
  image `seq` return (runIt, showIt)

main = runBenchmark buildIt

