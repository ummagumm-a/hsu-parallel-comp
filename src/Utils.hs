module Utils where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU 

vecToMatN1 :: Elt a => Acc (Vector a) -> Acc (Matrix a)
vecToMatN1 vec = A.reshape (A.lift $ Z :. n :. (1 :: Int)) vec
  where
    n = vecLen vec

vecLen :: Elt a => Acc (Vector a) -> Int
vecLen vec = expToSimple $ A.unindex1 (A.shape vec) :: Int

vecToMat1N :: Elt a => Acc (Vector a) -> Acc (Matrix a)
vecToMat1N vec = A.reshape (A.lift $ Z :. (1 :: Int) :. n) vec
  where
    n = vecLen vec

expToSimple :: Elt a => Exp a -> a
expToSimple expA = linearIndexArray (CPU.run (A.unit expA)) 0

nEls :: Elt a => Acc (Vector a) -> Int
nEls vec = expToSimple $ A.shapeSize $ A.shape vec

extVecAlong 
  :: Elt a
  => Int
  -> Acc (Vector a)
  -> Acc (Vector a)
extVecAlong n vec 
  = A.flatten $ A.replicate (constant (Z :. All :. n)) vec

findMax 
  :: (Shape sh, Elt a, A.Ord a)
  => Acc (Array sh a) 
  -> a
findMax arr = expToSimple $ A.maximum (A.flatten arr) A.!! 0

extVecAcross 
  :: Elt a
  => Int
  -> Acc (Vector a)
  -> Acc (Vector a)
extVecAcross n vec 
  = A.flatten $ A.replicate (constant (Z :. n :. All)) vec

maxes 
  :: (Elt a, Ord a)
  => Acc (Matrix a)
  -> Acc (Vector a)
maxes = fold1 A.max

