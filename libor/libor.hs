{-
# Redistribution and use in source and binary forms, with or without modification, are permitted
# provided that the following conditions are met:
# 1.   Redistributions of source code must retain the above copyright notice, this list of
# conditions and the following disclaimer.
# 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
# conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
# OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction, CPP #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import Harness  
import Data.Array.Repa as R hiding ((!))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
--import Data.List
import Control.Monad as M
import Control.Monad.ST as MST
import Control.Monad.Primitive
import System.IO
import System.IO.Unsafe
import Text.Printf

type LiborFloatArray = Array U DIM1 Float
type LiborFloatVector = U.Vector Float
type LiborFloatMVector m = U.MVector (PrimState m) Float
type LiborBSVector = U.Vector (Float, Float)

#ifdef SAFE
(!)=(U.!)
mvRead=UM.read
mvWrite=UM.write
vSlice=U.slice
#else
(!)=U.unsafeIndex
mvRead=UM.unsafeRead
mvWrite=UM.unsafeWrite
vSlice=U.unsafeSlice
#endif

maturities = U.fromList [4,4,4,8,8,8,20,20,20,28,28,28,40,40,40]
swaprates  = U.fromList [0.045, 0.05, 0.055, 0.045, 0.05, 0.055, 0.045, 0.05, 0.055, 0.045, 0.05, 0.055, 0.045, 0.05, 0.055]
delta = 0.25
l0val = 0.051
nmat = 40
liborN = nmat + 40

createVscalInner :: Float -> Int -> Float -> LiborBSVector -> Float
createVscalInner !curVscal !maturity !swaprate !ppnBS =
    let k = maturity - 1
        (b, s) = ppnBS ! k
#ifdef DEBUG
        (b0, s0) = ppnBS ! 0
#endif
        swapval = b + (swaprate * s) - 1.0
#ifdef DEBUG
    in unsafePerformIO (putStrLn $ (Text.Printf.printf "vscal: %f, mat: %d, swap: %f, b: %f, s: %f, val: %f, ppnBSlen: %d, b0: %f, s0: %f" curVscal maturity swaprate b s swapval (U.length ppnBS) b0 s0)) `seq`
       if swapval < 0.0 then curVscal + (swapval * (-100.0))
       else curVscal
#else
    in if swapval < 0.0 then curVscal + (swapval * (-100.0))
       else curVscal
#endif

createVscal :: LiborBSVector -> Float
createVscal !ppnBS =
    U.foldl' (\x y -> createVscalInner x (fst y) (snd y) ppnBS) 0.0 (U.zip maturities swaprates)

discount :: Float -> LiborFloatVector -> Float
discount !prediscount !l = 
#ifdef DEBUG
    U.foldl' (\x y -> unsafePerformIO (putStrLn $ (Text.Printf.printf "x: %f, y: %f, res: %f" x y (x / (1.0 + delta * y)))) `seq` x / (1.0 + delta * y)) prediscount l
#else
    U.foldl' (\x y -> x / (1.0 + delta * y)) prediscount l
#endif

buildBS :: LiborFloatVector -> LiborBSVector
buildBS !l = 
#ifdef DEBUG
    U.postscanl' (\(b, s) x -> unsafePerformIO (putStrLn $ (Text.Printf.printf "b: %f, s: %f, x: %f" b s x)) `seq`
      let newb = b / (1.0 + delta * x)
          news = s + delta * newb
      in (newb, news)) (1.0, 0.0) l
#else
    U.postscanl' (\(b, s) x -> 
      let newb = b / (1.0 + delta * x)
          news = s + delta * newb
      in (newb, news)) (1.0, 0.0) l
#endif

#ifdef DEBUG
buildLiiMV :: Int -> Int -> Int -> Float -> Float -> LiborFloatMVector (ST s) -> ST s ()
buildLiiMV !j !bindex !blength !vscal !sqez !l = do
#else
buildLiiMV :: Int -> Int -> Float -> Float -> LiborFloatMVector (ST s) -> ST s ()
buildLiiMV !bindex !blength !vscal !sqez !l = do
#endif
    if bindex == blength then return ()
    else do
        lval <- mvRead l bindex
        let lambda = 0.2
            con1 = delta * lambda
            newVscal = vscal + ((con1 * lval) / (1.0 + delta * lval))
            --vrat = (con1 * newVscal) + (lambda * (sqez - (0.5 * con1)))
            vrat = exp ( (con1 * newVscal) + (lambda * (sqez - (0.5 * con1))))
            nval = lval * vrat
#ifdef DEBUG
        unsafePerformIO (putStrLn $ (Text.Printf.printf "j: %d, i: %d, vscal: %f, vrat: %f, L[i]: %f new L[i]: %f" j bindex newVscal vrat lval nval)) `seq` mvWrite l bindex nval
        buildLiiMV j (bindex + 1) blength newVscal sqez l
#else
        mvWrite l bindex nval
        buildLiiMV (bindex + 1) blength newVscal sqez l
#endif

buildLinner :: Int -> Float -> LiborFloatMVector (ST s) -> ST s ( Int )
buildLinner !j !zval !l = do
    let sqez = (sqrt delta) * zval
#ifdef DEBUG
    buildLiiMV j (j + 1) (UM.length l) 0.0 sqez l
#else
    buildLiiMV (j + 1) (UM.length l) 0.0 sqez l
#endif
    return ( j + 1 )

buildL :: Int -> LiborFloatMVector (ST s) -> LiborFloatVector -> ST s ()
buildL !path !l !z = do
    _ <- U.foldl' (\x y -> x >>= \x' -> buildLinner x' y l) (return 0) ( vSlice (nmat * path) nmat z )
    return ()

pathCalc :: Int -> LiborFloatVector -> Float
pathCalc !path !z = runST $ do
    initL <- UM.replicate liborN (l0val :: Float)
    buildL path initL z
    realL <- U.unsafeFreeze initL
    let (lowL, highL) = U.splitAt nmat realL 
    let ppnBS = buildBS highL
    let initialVscal = createVscal ppnBS
    let res = discount initialVscal lowL
    return res

advance :: Int -> Int -> LiborFloatVector -> IO LiborFloatArray
advance !iterations !npath !z = do
    let fn i@(Z :. i') = pathCalc i' z
    res <- computeP $ fromFunction (Z :. npath) fn
    if iterations == 0 then return res
    else advance (iterations-1) npath z

getLines = M.liftM lines . readFile

doOutput dList = do
    ofile <- openFile "libor.res" WriteMode
    let sList = Prelude.map show dList
    mapM_ (\r -> hPutStrLn ofile r) sList
    hClose ofile

buildIt args = do
    let (iterations, npath) = case args of
             []          -> (1::Int, 921600::Int)
             [iterinput] -> ((read iterinput)::Int, 921600::Int)
             [iterinput, li] -> ((read iterinput)::Int, (read li)::Int)
    z <- getLines "z.input"
    let zfloat = U.fromList $ (take (npath * nmat) (repeat (read (head z) :: Float)))
    let runIt = advance (iterations-1) npath zfloat
    let showIt = Just (\r -> doOutput (toList r))
    zfloat `seq` return (runIt, showIt)

main = runBenchmark buildIt
