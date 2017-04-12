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
{-# LANGUAGE TypeOperators #-}

import Harness
import GHC.Float (float2Int, int2Float, int2Double)
import Control.DeepSeq (deepseq)
import Control.Monad (liftM2, (>=>))
import Data.Array.Repa as R hiding (map, (++), (!))
import Data.Bits (unsafeShiftR, unsafeShiftL, (.|.), (.&.))
import Data.Word

(!) :: Shape sh => Source r e => Array r sh e -> sh -> e
(!) = R.unsafeIndex

cotTheta = 1.6 :: Float

(.<.) :: Word -> Int -> Word
(.<.) = unsafeShiftL 

(.>.) :: Word -> Int -> Word
(.>.) = unsafeShiftR

word2Int :: Word -> Int
word2Int = fromIntegral

int2Word :: Int -> Word
int2Word = fromIntegral

word2Float :: Word -> Float
word2Float = int2Float . word2Int

data Vol3D = Vol3D !Int !Word !Word !Word deriving (Eq, Show)

instance Shape Vol3D where
  rank _ = 3
  zeroDim = Vol3D 0 0 0 0
  size (Vol3D n _ _ _) = word2Int (x * x * x) where x = 1 .<. n
  sizeIsValid (Vol3D n _ _ _) = n >= 0 && n <= 12
  toIndex _ (Vol3D n x y z) = word2Int (x .<. (n * 2) .|. (y .<. n) .|. z)
  fromIndex (Vol3D n _ _ _) j = Vol3D n x y z
    where x = (i .>. (n * 2)) .&. mask
          y = (i .>. n) .&. mask
          z = i .&. mask
          mask = (1 .<. n) - 1
          i = int2Word j
  deepSeq (Vol3D _ _ _ _) x = x
  -- the rest is left out as they are not interesting
  inShapeRange = undefined
  listOfShape = undefined
  shapeOfList = undefined
  unitDim = undefined
  intersectDim = undefined
  addDim = undefined

vol3D n = Vol3D n 0 0 0

type Matrix2D = R.Array U (Z :. Int :. Int) Float
type Matrix3D = R.Array U Vol3D Float

normalize u = R.fromListUnboxed three [x / q, y / q, z / q]
  where [x, y, z] = R.toList u
        q = sqrt $ x * x + y * y + z * z

dot u v = x * l + y * m + z * n
  where [x, y, z] = R.toList u
        [l, m, n] = R.toList v

cross u v = R.fromListUnboxed three [y * n - z * m, - (x * n - z * l), x * m - y * l]
  where [x, y, z] = R.toList u
        [l, m, n] = R.toList v

mult u v = R.fromFunction fourByFour $
  \ (Z :. i :. j) -> (u ! (Z :. i :. 0)) * (v ! (Z :. 0 :. j)) +
                     (u ! (Z :. i :. 1)) * (v ! (Z :. 1 :. j)) +
                     (u ! (Z :. i :. 2)) * (v ! (Z :. 2 :. j)) +
                     (u ! (Z :. i :. 3)) * (v ! (Z :. 3 :. j))

fourByFour = Z :. (4 :: Int) :. (4 :: Int) 
three = Z :. (3 :: Int)

backProjection :: Int -> Matrix2D -> Matrix2D -> IO Matrix3D
backProjection volBits projectionMatrix raw = R.computeP $ R.fromFunction (vol3D volBits) f
  where 
    c00 = projectionMatrix ! (Z :. 0 :. 0)
    c01 = projectionMatrix ! (Z :. 0 :. 1)
    c02 = projectionMatrix ! (Z :. 0 :. 2)
    c03 = projectionMatrix ! (Z :. 0 :. 3)
    c10 = projectionMatrix ! (Z :. 1 :. 0)
    c11 = projectionMatrix ! (Z :. 1 :. 1)
    c12 = projectionMatrix ! (Z :. 1 :. 2)
    c13 = projectionMatrix ! (Z :. 1 :. 3)
    c20 = projectionMatrix ! (Z :. 2 :. 0)
    c21 = projectionMatrix ! (Z :. 2 :. 1)
    c22 = projectionMatrix ! (Z :. 2 :. 2)
    c23 = projectionMatrix ! (Z :. 2 :. 3)
    c30 = projectionMatrix ! (Z :. 3 :. 0)
    c31 = projectionMatrix ! (Z :. 3 :. 1)
    c32 = projectionMatrix ! (Z :. 3 :. 2)
    c33 = projectionMatrix ! (Z :. 3 :. 3)

    f (Vol3D _ i j k) = w * w * 
      ((1 - wl) * ((1 - wm) * (raw ! (Z :. l       :. m)) + wm * (raw ! (Z :. l       :. (m + 1)))) +
             wl * ((1 - wm) * (raw ! (Z :. (l + 1) :. m)) + wm * (raw ! (Z :. (l + 1) :. (m + 1)))))
      where 
        ri = word2Float i
        rj = word2Float j
        rk = word2Float k
        tmp0 = c00 * ri + c01 * rj + c03
        tmp1 = c10 * ri + c11 * rj + c13
        tmp3 = c30 * ri + c31 * rj + c33
        w = 1 / (tmp3 + c32 * rk)
        rl = w * (tmp1 + c12 * rk)
        rm = w * (tmp0 + c02 * rk)
        l = float2Int rl 
        m = float2Int rm
        wl = rl - int2Float l
        wm = rm - int2Float m

checkSum :: Int -> Matrix3D -> IO Double
checkSum volBits vol = R.sumAllP $ R.fromFunction (vol3D volBits) f
  where 
    f sh = realToFrac (vol ! sh) * fromIntegral (toIndex sh sh `mod` 11111) 

prepareData :: Int -> IO (Matrix2D, Matrix2D)
prepareData volBits = 
  liftM2 (,) (R.computeP projectionMatrix) (R.computeP raw)
  where
    volDIM  = 1 `unsafeShiftL` volBits :: Int
    winBits = volBits + 1
    winX    = 2 ^ winBits 
    winY    = 2 ^ winBits
    imgSize = volDIM * volDIM * volDIM 
    eye    = R.fromListUnboxed three $ map fromIntegral [-volDIM, -volDIM, -256]
    center = R.fromListUnboxed three $ map fromIntegral [volDIM, volDIM, volDIM]
    near   = fromIntegral volDIM * sqrt 3
    far    = near + fromIntegral volDIM * 2 * sqrt 3 
    up     = R.fromListUnboxed three [0, 0, 1] 

    raw = R.fromFunction (Z :. winY :. winX) $ \ (Z :. y :. x) -> 
            fromIntegral (1 + x + y * winX) / fromIntegral (winX * winY) 

    n = normalize $ R.zipWith (-) eye center
    proj = up `dot` n
    v = normalize $ R.zipWith (\x y -> x - proj * y) up n
    u = v `cross` n
    [u0, u1, u2] = R.toList u
    [v0, v1, v2] = R.toList v
    [n0, n1, n2] = R.toList n

    modelView = R.fromListUnboxed fourByFour
          [ u0, u1, u2, - eye `dot` u,
            v0, v1, v2, - eye `dot` v,
            n0, n1, n2, - eye `dot` n,
            0 ,  0,  0, 1 ]
    viewPort = R.fromListUnboxed fourByFour
      [ fromIntegral winX / 2, 0, 0, fromIntegral winX / 2,
        0, fromIntegral winY / 2, 0, fromIntegral winY / 2,
        0, 0, 0.5, 0.5,
        0, 0, 0, 1 ]
    projection = R.fromListUnboxed fourByFour
      [ cotTheta, 0, 0, 0,
        0, cotTheta, 0, 0,
        0, 0, (far + near) / (near - far), 2 * far * near / (near - far),
        0, 0, -1, 0 ]
    projectionMatrix = viewPort `mult` (projection `mult` modelView) 
 
main = runBenchmark buildIt

buildIt args = do
  putStrLn $ "Running back projection of volBits = " ++ show volBits
  (projectionMatrix, raw) <- prepareData volBits 
  projectionMatrix `seq` raw `seq` return (backProjection volBits projectionMatrix raw, showIt)
  where
    volBits =
      case args of 
        []      -> 8
        x:[]    -> case reads x of
                     [(b, [])] -> b
                     _ -> error "Usage: backprojection <volBits>"
    showIt = Just (checkSum volBits >=> 
                   writeFile "backprojection.res" . ("Final checksum: "++) . show)
