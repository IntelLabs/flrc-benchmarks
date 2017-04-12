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
-- Modified for use with Repa and HRC by Todd Anderson (2012)
-}

import Harness
import Control.Monad
import System.Environment
import System.IO.Unsafe
import Data.Array.Repa (U, Z(..), (:.)(..), DIM1, Array)
import qualified Data.Array.Repa as R 

-- Option data structure:
-- (spotPrice, strike, rate, volatility, expiration, optionType)
-- where optionType is 1.0 when it is PUT call, otherwise 0.0
type OptionData = (Float, Float, Float, Float, Float, Float)

inv_sqrt_2xPI = 0.39894228040143270286
cndf_C1 = 0.2316419
cndf_C2 = 0.319381530
cndf_C3 = -0.356563782
cndf_C4 = 1.781477937
cndf_C5 = -1.821255978
cndf_C6 = 1.330274429

cndf :: Float -> Float
cndf inputX = pos - sig * outputX
  where 
    {-# NOINLINE pos #-}
    pos = if inputX > 0 then 1 else 0
    sig = pos + pos - 1
    inputX' = sig * inputX 
    tmp = inv_sqrt_2xPI * exp(-0.5 * inputX' * inputX')
    k2  = 1.0 / (1.0 + cndf_C1 * inputX')
    k2_2 = k2   * k2
    k2_3 = k2_2 * k2
    k2_4 = k2_3 * k2
    k2_5 = k2_4 * k2
    outputX = tmp * (cndf_C2 * k2   +
                     cndf_C3 * k2_2 +
                     cndf_C4 * k2_3 +
                     cndf_C5 * k2_4 +
                     cndf_C6 * k2_5)

blkSchlsEqEuroNoDiv :: OptionData -> Float
blkSchlsEqEuroNoDiv (spotPrice, strike, rate, volatility, expiration, optionType)
 = optionPrice'
 where 
   logTerm = log (spotPrice / strike)                
   powerTerm = 0.5 * volatility * volatility
   den = volatility * sqrt(expiration)
   d1 = (((rate + powerTerm) * expiration) + logTerm) / den
   d2 = d1 - den
   nofXd1 = cndf d1
   nofXd2 = cndf d2
   futureValue = strike *  exp (-rate * expiration)
   optionPrice  = (spotPrice * nofXd1) - (futureValue * nofXd2)
   optionPrice' = optionPrice + optionType * (futureValue - spotPrice)

blackscholes src = R.computeUnboxedP $ R.map blkSchlsEqEuroNoDiv src

readOptionData :: Int -> String -> IO (R.Array R.U R.DIM1 OptionData)
readOptionData size optionFile = do
  s <- readFile optionFile
  let dat = map readData $ filter isData $ lines s
      datSize = length dat
      raw = R.fromListUnboxed (Z :. datSize) dat
      f (Z :. i) = raw `R.unsafeIndex` (Z :. (i `mod` datSize))
  R.computeP $ R.fromFunction (Z :. size) f
  where
    readData s = (toFloat p, toFloat q, toFloat r, toFloat v, toFloat e, toBool o)
      where [p, q, r, v, e, o] = words s
    toBool ('"':'P':_) = 1
    toBool _ = 0
    toFloat = read . keepFloat
    keepFloat (c:s) | c >= '0' && c <= '9' || c == '.' = c : keepFloat s
                    | otherwise = keepFloat s
    keepFloat []  = []
    isData ('{':_) = True
    isData _ = False

main = runBenchmark buildIt

buildIt args = do
  arr <- readOptionData numOptions "optionData.txt"
  putStrLn $ "Running blackscholes, numOptions " ++ show numOptions 
  return (blackscholes arr, showIt)
  where
    numOptions =
      case args of 
        []      -> 1000000
        _       -> read $ last args
    showIt = Just (R.sumAllP >=> writeFile "blackscholes.res" . ("Final checksum: "++) . show)
