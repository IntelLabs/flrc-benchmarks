{-
-- Intel Concurrent Collections for Haskell
-- Copyright (c) 2010, Intel Corporation.
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms and conditions of the GNU Lesser General Public License,
-- version 2.1, as published by the Free Software Foundation.
--
-- This program is distributed in the hope it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
-- more details.
--
-- You should have received a copy of the GNU Lesser General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 
-- 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.

-- Ported from CnC/C++ program by Ryan Newton
-- Modified for use with Repa and HRC by Leaf Petersen (2012)
-}

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Harness  
import Control.Monad
import Data.Int
import qualified Data.List as List
import qualified Data.Array.Repa as R
import GHC.Exts
import System.Environment
import Control.DeepSeq
import qualified Text.Printf as T

type Float3D = (Float, Float, Float)
type PVector = R.Array R.U R.DIM1 Float3D
-- This step generates the bodies in the system.
genVector sh tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral (R.toIndex sh tag)

-- This step computes the accelerations of the bodies.       
compute :: PVector -> Float3D -> Float3D
compute vecList myvector = next
       where
             next = accel myvector vecList
             g = 9.8
             multTriple :: Float -> Float3D -> Float3D
             multTriple c ( x,y,z ) = ( c*x,c*y,c*z )

             pairWiseAccel :: Float3D -> Float3D -> Float3D
             pairWiseAccel (x,y,z) (x',y',z') = let dx = x'-x
                                                    dy = y'-y
                                                    dz = z'-z
                                                    eps = 0.005
                                                    distanceSq = dx*dx + dy*dy + dz*dz + eps
                                                    factor = 1/sqrt(distanceSq * distanceSq * distanceSq)
                                                    r0 = factor * dx
                                                    r1 = factor * dy
                                                    r2 = factor * dz
                                                    r = (r0, r1, r2)
                                                in r
             {-# INLINE sumTriples #-}
             sumTriples = R.foldAllS (\(!x,!y,!z) (!x',!y',!z') -> (x+x',y+y',z+z')) (0,0,0)
             accel vector vecList = multTriple g $ sumTriples $ R.map (pairWiseAccel vector) vecList


advance :: Int -> PVector -> PVector
advance n accels = if n == 0 then accels 
                   else let step = R.computeUnboxedS (R.map (compute accels) accels)
                        in advance (n-1) step

run :: Int -> Int -> PVector
run n iterations = advance iterations (R.computeUnboxedS (R.fromFunction d $ genVector d))
  where d = R.ix1 n
buildIt args =  return (runIt, showIt)
  where
    (s, i) = case args of 
               []     -> (3::Int, 10::Int)
               [s]    -> (read s, 10::Int)
               [s, i] -> (read s, read i)
    runIt = do
      let acc = run s i
      acc `R.deepSeqArray` (return acc)
    showIt = 
      let f = \r -> 
                  let s = List.concat ([ T.printf "(%.3g, %.3g %.3g)\t" x y z | (x, y, z) <- (R.toList r) ])
                  in writeFile "nbody-repa.res" s
      in Just f

main = runBenchmark buildIt
