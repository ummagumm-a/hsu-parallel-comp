module Lib
    ( someFunc
    ) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Control.Lens as Lens
import qualified Prelude as P
import Data.Char
import Utils
import Ops

-- TODO: write function which operates 
-- on a vector and each row of a matrix
-- Optionally - extend to any two arrays

depthVec :: Acc (Vector Int)
depthVec = A.use $ fromList (Z:.9) [0,1,2,3,4,4,4,2,3]

nodeVec :: Acc (Vector Char) 
nodeVec = A.use $ fromList (Z:.9) "FEFEVPVAN"

valuesVec :: Acc (Vector Char)
valuesVec = A.use $ fromList (Z:.9) "f000w+w07" 

fASTMatrix 
  :: Acc (Vector Int)
  -> Acc (Vector Char)
  -> Acc (Vector Char)
  -> Acc (Matrix Char)
fASTMatrix depths nodes values 
  = vecToMatN1 depths' 
    A.++ vecToMatN1 nodes 
    A.++ vecToMatN1 values
  where
    depths' = A.map (A.chr . (+ 48)) depths

depthMat 
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
depthMat vec = A.map A.boolToInt $ outerVecProduct vec (A.==) depths
  where
    depths = A.use (fromList (Z:.depth) [0..] :: Vector Int)

    depth = 1 + findMax vec

interestingMat 
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
interestingMat mat = A.transpose $ A.scanl1 (+) (A.transpose mat)

takeNForNC 
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
takeNForNC vec mat
  = helper 0
  where
    maxDep = findMax vec

    (n1,n2) = expToSimple $ A.unindex2 (A.shape mat)
   
    helper k
      | k P.== (n1 - 1) = resVec
      | otherwise = concatOn _2 resVec (helper (k + 1))
        where
          resVec = vecToMat1N $ A.take vecEl row ++ zeros
          vecEl = (A.!!) vec (A.lift k)
            
          row = A.slice mat (A.constant (Z :. (k :: Int) :. All))
          zeros = A.fill (A.constant (Z :. (maxDep - expToSimple vecEl))) 0 :: Acc (Vector Int)

matShape 
  :: Elt a 
  => Acc (Matrix a) 
  -> Exp (Int, Int)
matShape mat = A.unindex2 (A.shape mat)

nodeCoords
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
nodeCoords vec
  = takeNForNC (A.map (+1) vec) (interestingMat (depthMat vec)) 

isPrefixOfMats
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
isPrefixOfMats mat1 mat2
  = A.map boolToInt $ innerProduct mat1 (A.&&) f (A.transpose mat2)
  where
    f = \a b -> a A.== b A.|| b A.== 0


nameCoords 
  :: (Elt a, Eq a)
  => Exp a 
  -> Acc (Vector a)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
nameCoords el names nc 
  = selectRows (A.map (A.== lift el) names) nc

nameC = nameCoords (lift 'F') nodeVec (nodeCoords depthVec)

dropLastNonZero 
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
dropLastNonZero mat
  = A.imap leaveOrZero mat 
  where
    Z :. _ :. cols = unlift (A.shape mat) :: Z :. Exp Int :. Exp Int

    leaveOrZero :: Exp (Z :. Int :. Int) -> Exp Int -> Exp Int
    leaveOrZero shape el 
      = ifThenElse zeroCond 0 el
          where
          row = shape ^. _2 
          col = shape ^. _1
          zeroCond = col A.== (cols - 1) A.&& el A./= 0
            A.|| col A./= cols && mat A.! lift (Z :. row :. (col + 1)) A.== 0

replaceOnesByCols 
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
replaceOnesByCols mat = A.zipWith (*) extVec mat
  where 
    shape = matShape mat
    rows = fst shape 
    cols = snd shape
    
    vec = enumFromN (lift $ Z :. cols) 0 :: Acc (Vector Int)
    extVec = A.replicate (lift $ Z :. rows :. All) vec

ancestors 
  :: Acc (Matrix Int)
  -> Acc (Vector Char)
  -> Exp Char
  -> Acc (Vector Int)
ancestors nc nv ch = maxes rep
  where
    dnc = dropLastNonZero nc
    names = nameCoords ch nv nc
    anc = isPrefixOfMats dnc names
    rep = replaceOnesByCols anc

    {-
concatAll 
  :: Elt a
  => Acc (Array DIM3 a)
  -> Acc (Matrix a)
concatAll arr = helper 0
  where
    n = A.shape arr ^. _3

    helper k 
      | k == (n - 1) = undefined
-}

ancMat
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
ancMat vec mat = A.reshape (lift (Z :. vecSize :. cols)) ancs
  where
    shape = matShape mat
    rows = fst shape 
    cols = snd shape

    exp = A.expand (const cols) (\p i -> p * cols + i) anc :: Acc (Vector Int)

    vecSize = A.length vec
    ancs = gather exp (A.flatten mat)
--    filtered = A.compact expMat repMat




someFunc :: P.IO ()
someFunc = P.print 2

mat :: Acc (Matrix Int)
mat = A.use (fromList (Z :. 2 :. 5) [0..])

vec :: Acc (Vector Int)
vec = A.use $ fromList (Z :. 5) [0..]

anc = ancestors (nodeCoords depthVec) nodeVec (lift 'F')

res = ancMat anc nameC

