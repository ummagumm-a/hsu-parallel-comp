module Utils where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU 

-- | Converts a vector into a one-column matrix.
vecToMatN1 :: Elt a => Acc (Vector a) -> Acc (Matrix a)
vecToMatN1 vec = A.reshape (A.lift $ Z :. n :. (1 :: Int)) vec
  where
    n = A.size vec

-- | Converts a vector into a one-row matrix.
vecToMat1N :: Elt a => Acc (Vector a) -> Acc (Matrix a)
vecToMat1N vec = A.reshape (A.lift $ Z :. (1 :: Int) :. n) vec
  where
    n = A.size vec

-- | Converts Exp of pair to pair of Exp's.
expToPair :: Elt i => Exp (i, i) -> (Exp i, Exp i)
expToPair e = (A.fst e, A.snd e)

-- | Returns the number of rows and columns of a matrix.
matSh 
  :: Elt a 
  => Acc (Matrix a) 
  -> (Exp Int, Exp Int)
matSh = sh2ToPair . A.shape

-- | Converts DIM2 shape into a pair of Exp Int's.
sh2ToPair 
  :: Exp DIM2
  -> (Exp Int, Exp Int)
sh2ToPair = expToPair . A.unindex2

-- | Returns the maximum element in the whole array.
findMax 
  :: (Shape sh, Elt a, A.Ord a)
  => Acc (Array sh a) 
  -> Exp a
findMax arr = A.maximum (A.flatten arr) A.!! 0

-- | Replicates each element of a vector n times.
-- >>> let vec = fromList (Z:.5) [0..] :: Vector Int
-- >>> vec
-- Vector (Z :. 5) [0,1,2,3,4]
-- 
-- >>> CPU.run $ extVecAlong 5 (A.use vec)
-- Vector (Z :. 15) [0,0,0,1,1,1,2,2,2,3,3,3,4,4,4]
extVecAlong 
  :: Elt a
  => Exp Int
  -> Acc (Vector a)
  -> Acc (Vector a)
extVecAlong n 
  = expand (const n) const

-- | Replicates vector n times.
--
-- >>> let vec = fromList (Z :. 3) [0..] :: Vector Int
-- >>> vec
-- Vector (Z :. 3) [0,1,2]
--
-- >>> CPU.run $ extVecAcross 4 (A.use vec)
-- Vector (Z :. 12) [0,1,2,0,1,2,0,1,2,0,1,2]
extVecAcross 
  :: Elt a
  => Exp Int
  -> Acc (Vector a)
  -> Acc (Vector a)
extVecAcross n vec 
  = A.flatten $ A.replicate (A.lift (Z :. n :. All)) vec
