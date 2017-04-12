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
import System.IO
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as U

type VRIntArray   = Array U DIM1 Int
type VRFloatArray = Array U DIM1 Float

computeColorInner :: Int -> Int -> Int -> VRIntArray -> VRFloatArray -> Float -> Float -> Float
computeColorInner !len !raylen !arrayIndex !visible !opacity !colorInner !opacityInner =
    let voxel = arrayIndex + len
    in if len < raylen then
--         if visible ! ( Z :. voxel ) == 0 then
         if visible `unsafeIndex` ( Z :. voxel ) == 0 then
           computeColorInner (len + 1) raylen arrayIndex visible opacity colorInner opacityInner 
         else
--           let curOpacity = opacity ! ( Z:. voxel )
           let curOpacity = opacity `unsafeIndex` ( Z:. voxel )
           in if curOpacity == 0.0 then
                computeColorInner (len + 1) raylen arrayIndex visible opacity colorInner opacityInner 
              else
                let shading     = curOpacity   + 1.0
                    colorPass   = colorInner   + shading    * (1.0 - opacityInner)
                    opacityPass = opacityInner + curOpacity * (1.0 - opacityInner)
                in if opacityPass > 0.0 then
                     colorPass
                   else
                     computeColorInner (len + 1) raylen arrayIndex visible opacity colorPass opacityPass 
       else 
         colorInner 

computeColor :: Int -> Int -> VRIntArray -> VRFloatArray -> Float 
computeColor !raylen !arrayIndex !visible !opacity =
    computeColorInner 0 raylen arrayIndex visible opacity 0.0 0.0 

advance :: Int -> Int -> VRIntArray -> VRIntArray -> VRFloatArray -> IO VRFloatArray
advance !iterations ilength !ray !visible !opacity = do
    let fn i@(Z :. i') = computeColor (ray `unsafeIndex` i) i' visible opacity
    res <- -- computeP $ traverse ray id (\f (Z :. i) -> computeColor ( f (Z :. i) ) i visible opacity)
           computeP $ fromFunction (Z :. ilength) fn
    newray <- computeP $ R.map (\r -> (r + 1) `mod` 40) ray
    if iterations == 0 then 
        return res
    else 
        res `seq` advance (iterations-1) ilength newray visible opacity 

parseN :: (U.Unbox a, Read a) => String -> Int -> IO (Array U DIM1 a)
parseN inputFile len = do
  h <- openFile inputFile ReadMode
  arr <- U.replicateM len (hGetLine h >>= readIO)
  hClose h
  return $ fromUnboxed (Z :. len) arr

doOutput dList = do
    ofile <- openFile "vr.res" WriteMode
    let sList = Prelude.map show dList
    mapM_ (\r -> hPutStrLn ofile r) sList
    hClose ofile
    
buildIt args = do
    let (iterations, ilength) = case args of
             []          -> (100000::Int, 8192::Int)
             [iterinput] -> ((read iterinput)::Int, 8192::Int)
             [iterinput, li] -> ((read iterinput)::Int, (read li)::Int)
    let vlength      = ilength + 40
    raydata <- parseN "raylen.input" ilength 
    visibledata <- parseN "visible.input" vlength 
    opacitydata <- parseN "opacity.input" vlength 
    let runIt = advance (iterations-1) ilength raydata visibledata opacitydata
    let showIt = Just (\r -> doOutput (toList r))
    raydata `seq` visibledata `seq` opacitydata `seq` return (runIt, showIt)
    
main = runBenchmark buildIt
