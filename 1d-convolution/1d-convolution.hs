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

{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import Harness  
import qualified Data.Array.Repa as R
import GHC.Float (int2Float)

type Complex = (Float, Float)
type Data = R.Array R.U R.DIM1 Complex
type Stencil = R.Array R.D R.DIM1 Float

complexPlus :: Complex -> Complex -> Complex
complexPlus !(!r0, !i0) !(!r1, !i1) = (r0 + r1, i0 + i1)

stencilSize = (8192::Int)
stencilShape = R.Z R.:. stencilSize :: R.DIM1

-- Assumes stencil size is a power of 2
genInput :: Int -> IO Data
genInput size = R.computeP $ R.fromFunction shape $ genOne 
  where
    shape =  R.Z  R.:. size + stencilSize :: R.DIM1
    padLeft = stencilSize `div` 2
    padRight = stencilSize `div` 2
    indexLeft = padLeft - 1
    indexRight = size + padRight - 1
    genOne tag = 
      let
        idx0 = R.toIndex shape tag
        idx  = if idx0 < indexLeft then indexLeft 
               else if idx0 > indexRight then indexRight 
               else idx0
        r    = fromIntegral ((2*idx) `mod` 7)
        i    = fromIntegral ((2*idx+1) `mod` 9)
      in (r, i)

checkSum :: Data -> Float
checkSum d = r + i
  where
    (r, i) = R.foldAllS complexPlus (0.0, 0.0) d

convolve0 :: Complex -> Float -> Complex
convolve0 !(!r, !i) s = (r*s - i*s, r*s + i*s)

convolve :: Int -> Stencil -> Data -> IO Data
convolve !size stencil input = R.computeUnboxedP $ R.fromFunction shape genOne
  where
    genOne tag = 
      let
        elements = R.extract tag stencilShape input
        partials = R.zipWith convolve0 elements stencil
        final = R.foldAllS complexPlus (0.0, 0.0) partials
      in final
    shape =  R.Z  R.:. size :: R.DIM1

run :: Int -> Stencil -> Int -> Data -> IO Data
run size stencil n input | n == 0    = return input
                         | otherwise = convolve size stencil input >>= run size stencil (n-1)

buildIt args = do
  input   <- genInput size
  let stencil = R.fromFunction stencilShape $ \_ -> 2
  let runIt  = run size stencil iterations input
      showIt = Just (writeFile "1d-convolution.res" . show . checkSum) 
  return (runIt, showIt)
  where
    (size, iterations) = case args of
             []     -> (1000000::Int, 1::Int)
             [s]    -> ((read s)::Int, 1::Int)
             [s, i] -> ((read s)::Int, (read i)::Int)

main = runBenchmark buildIt
