module Lib where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Char
import Utils
import Ops

-- TODO: write function which operates 
-- on a vector and each row of a matrix
-- Optionally - extend to any two arrays

depthVec :: Acc (Vector Int)
depthVec = A.use $ fromList (Z:.9) [0,1,2,3,4,4,4,2,3]

depthVec' :: Acc (Vector Int)
depthVec' = A.use $ fromList (Z:.10) [0,1,2,2,2,1,1,2,2,2]

nodeVec :: Acc (Vector Char) 
nodeVec = A.use $ fromList (Z:.9) "FEFEVPVAN"

nodeVec' :: Acc (Vector Char) 
nodeVec' = A.use $ fromList (Z:.10) "EEVPVPEVPV"

valuesVec :: Acc (Vector Char)
valuesVec = A.use $ fromList (Z:.9) "f000w+w07" 

valuesVec' :: Acc (Vector Char)
valuesVec' = A.use $ fromList (Z:.10) "v0a+b/0c*d"


-- | Composes vector of depths, vector of node names
-- and vector of values at nodes into a single matrix.
--
-- >>> CPU.run depthVec
-- Vector (Z :. 9) [0,1,2,3,4,4,4,2,3]
-- >>> CPU.run nodeVec
-- Vector (Z :. 9) ['F','E','F','E','V','P','V','A','N']
-- >>> CPU.run valuesVec
-- Vector (Z :. 9) ['f','0','0','0','w','+','w','0','7']
--
-- CPU.run $ fASTMatrix depthVec nodeVec valuesVec
-- Matrix (Z :. 9 :. 3) 
--   [ '0', 'F', 'f',
--     '1', 'E', '0',
--     '2', 'F', '0',
--     '3', 'E', '0',
--     '4', 'V', 'w',
--     '4', 'P', '+',
--     '4', 'V', 'w',
--     '2', 'A', '0',
--     '3', 'N', '7']
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

-- | Produces depth matrix for a depth vector.
-- Each row corresponds to each element (el) in vector.
-- Non-zero element is located at the column of number el.
--
-- >>> CPU.run depthVec
-- Vector (Z :. 9) [0,1,2,3,4,4,4,2,3]
--
-- >>> CPU.run $ depthMat depthVec
-- Matrix (Z :. 9 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     0, 1, 0, 0, 0,
--     0, 0, 1, 0, 0,
--     0, 0, 0, 1, 0,
--     0, 0, 0, 0, 1,
--     0, 0, 0, 0, 1,
--     0, 0, 0, 0, 1,
--     0, 0, 1, 0, 0,
--     0, 0, 0, 1, 0]
--
depthMat 
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
depthMat vec 
  = outerVecProduct vec g depths
  where
    depths = A.enumFromN (A.lift $ Z :. depth) 0

    g = \x y -> A.boolToInt $ x A.== y

    depth = 1 + findMax vec

-- | Produces an interesting result of applying 
-- a sum scan along the first axis to the depth matrix.
interestingMat 
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
interestingMat = A.transpose . A.scanl1 (+) . A.transpose

-- | Takes n_i elements from each i-th row of a matrix.
-- >>> let vec = fromList (Z :. 3) [0..] :: Vector Int
-- >>> *Lib> vec
-- Vector (Z :. 3) [0,1,2]
--
-- >>> let mat = fromList (Z :. 3 :. 4) [0..] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 3 :. 4) 
--   [ 0, 1,  2,  3,
--     4, 5,  6,  7,
--     8, 9, 10, 11]
--
-- >>> CPU.run $ takeNFromRows (A.use vec) (A.use mat)
-- Matrix (Z :. 3 :. 4) 
--   [ 0, 0,  0, 0,
--     4, 5,  0, 0,
--     8, 9, 10, 0]
takeNFromRows
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
takeNFromRows vec = A.imap f
  where
    f sh el = ifThenElse (x A.> (vec !! y)) 0 el
      where
        (I2 y x) = sh

takeNFromRows'
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
takeNFromRows' vec mat
  = izipWith f dMat mat
  where
    (I2 rows cols) = shape mat
    
    dMat = A.reshape (A.lift $ Z :. rows :. cols) 
         $ extVecAlong cols vec 
    
    f sh x y = let (I2 _ col) = sh 
                in ifThenElse (col A.> x) 0 y


-- | Produces the node coordinates for a given depth vector.
-- >>> CPU.run depthVec
-- Vector (Z :. 9) [0,1,2,3,4,4,4,2,3]
--
-- >>> CPU.run $ nodeCoords depthVec
-- Matrix (Z :. 9 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     1, 1, 0, 0, 0,
--     1, 1, 1, 0, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 1, 1, 1,
--     1, 1, 1, 1, 2,
--     1, 1, 1, 1, 3,
--     1, 1, 2, 0, 0,
--     1, 1, 2, 2, 0]
nodeCoords
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
nodeCoords vec
  = takeNFromRows' vec (interestingMat (depthMat vec)) 

-- | Selects coordinates of nodes with a given name.
--
-- >>> CPU.run nodeVec
-- Vector (Z :. 9) ['F','E','F','E','V','P','V','A','N']
--
-- >>> CPU.run $ nodeCoords depthVec
-- Matrix (Z :. 9 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     1, 1, 0, 0, 0,
--     1, 1, 1, 0, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 1, 1, 1,
--     1, 1, 1, 1, 2,
--     1, 1, 1, 1, 3,
--     1, 1, 2, 0, 0,
--     1, 1, 2, 2, 0]
--
-- >>> CPU.run $ nameCoords (A.lift 'F') nodeVec (nodeCoords depthVec)
-- Matrix (Z :. 2 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     1, 1, 1, 0, 0]
nameCoords 
  :: (Elt a, Eq a)
  => Exp a 
  -> Acc (Vector a)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
nameCoords el names nc 
  = selectRows (A.map (A.== lift el) names) nc
  where
    f (Z :. i) x = ifThenElse (x A.== lift el) i 0

nameCoords'
  :: (Elt a, Eq a)
  => Exp a 
  -> Acc (Vector a)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
nameCoords' el names 
  = selectRows' (A.afst $ A.filter (A./= (-1)) $ A.imap f names)
  where
    f sh x = ifThenElse (x A.== el) i (-1) 
      where
        i = A.unindex1 sh :: Exp Int


-- | Given two matrices, returns a matrix 
-- where each entry (i,j) is 1 if i'th row of mat1
-- is a prefix of j'th row of mat2.
--
-- >>> let mat = fromList (Z :. 2 :. 5) [1,0,0,0,0,1,1,1,0,0] :: Matrix Int
-- >>> mat
-- Matrix (Z :. 2 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     1, 1, 1, 0, 0]
--
-- >>> CPU.run mat2
-- Matrix (Z :. 9 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     1, 1, 0, 0, 0,
--     1, 1, 1, 0, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 1, 1, 1,
--     1, 1, 1, 1, 2,
--     1, 1, 1, 1, 3,
--     1, 1, 2, 0, 0,
--     1, 1, 2, 2, 0]
--
-- >>> CPU.run $ isPrefixOfMats mat2 mat1
-- Matrix (Z :. 9 :. 2) 
--   [ 1, 0,
--     1, 0,
--     1, 1,
--     1, 1,
--     1, 1,
--     1, 1,
--     1, 1,
--     1, 0,
--     1, 0]
isPrefixOfMats
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
isPrefixOfMats mat1 mat2
  = A.map A.boolToInt 
  $ innerProduct mat1 (A.&&) f (A.transpose mat2)
  where
    f = \a b -> a A.== b A.|| b A.== 0

-- | Drops last non-zero elements in a matrix.
--
-- >>> CPU.run mat
-- Matrix (Z :. 9 :. 5) 
--   [ 1, 0, 0, 0, 0,
--     1, 1, 0, 0, 0,
--     1, 1, 1, 0, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 1, 1, 1,
--     1, 1, 1, 1, 2,
--     1, 1, 1, 1, 3,
--     1, 1, 2, 0, 0,
--     1, 1, 2, 2, 0]
--
-- >>> CPU.run $ dropLastNonZero mat
-- Matrix (Z :. 9 :. 5) 
--   [ 0, 0, 0, 0, 0,
--     1, 0, 0, 0, 0,
--     1, 1, 0, 0, 0,
--     1, 1, 1, 0, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 1, 1, 0,
--     1, 1, 0, 0, 0,
--     1, 1, 2, 0, 0]
dropLastNonZero 
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
dropLastNonZero mat
  = A.imap leaveOrZero mat 
  where
    (I2 _ cols) = shape mat

    leaveOrZero :: Exp (Z :. Int :. Int) -> Exp Int -> Exp Int
    leaveOrZero sh el 
      = ifThenElse zeroCond 0 el
          where
          (I2 row col) = sh
          zeroCond = col A.== (cols - 1) A.&& el A./= 0
            A.|| col A./= cols && mat A.! lift (Z :. row :. (col + 1)) A.== 0

dropLastNonZero'
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
dropLastNonZero' depths 
  = takeNFromRows' (A.map (\x -> x - 1) depths)

-- | Given a matrix with 0 and 1 elements, 
-- replaces each 1 by the number of its column.
--
-- >>> CPU.run mat
-- Matrix (Z :. 9 :. 3) 
--   [ 1, 0, 1,
--     1, 0, 0,
--     1, 1, 1,
--     1, 1, 0,
--     1, 1, 0,
--     1, 1, 0,
--     1, 1, 1,
--     1, 0, 1,
--     1, 0, 0]
--
-- >>> CPU.run $ replaceOnesByCols mat
-- Matrix (Z :. 9 :. 3) 
--   [ 0, 0, 2,
--     0, 0, 0,
--     0, 1, 2,
--     0, 1, 0,
--     0, 1, 0,
--     0, 1, 0,
--     0, 1, 2,
--     0, 0, 2,
--     0, 0, 0]
replaceOnesByCols 
  :: Acc (Matrix Int)
  -> Acc (Matrix Int)
replaceOnesByCols mat = A.zipWith (*) extVec mat
  where 
    (I2 rows cols) = shape mat
    
    vec = enumFromN (lift $ Z :. cols) 0 :: Acc (Vector Int)
    extVec = A.replicate (lift $ Z :. rows :. All) vec

-- | Returns the number of the closest ancestor for each node.
--
-- >>> CPU.run $ ancestors (nodeCoords depthVec) nodeVec (lift 'F')
-- Vector (Z :. 9) [0,0,0,1,1,1,1,0,0]
ancestors 
  :: Acc (Vector Int)
  -> Acc (Vector Char)
  -> Exp Char
  -> Acc (Vector Int)
ancestors ds nv ch = A.maximum rep
  where
    nc = nodeCoords ds
    dnc = dropLastNonZero' ds nc
    names = nameCoords' ch nv nc
    anc = isPrefixOfMats dnc names
    rep = replaceOnesByCols anc

-- | Returns a matrix where each row 
-- is the coordinate of the closest ancestor 
-- to each corresponding row of the given matrix.
ancMat
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
ancMat vec mat = A.reshape (lift (Z :. vecSize :. cols)) ancs
  where
    (I2 rows cols) = shape mat

    exp = A.expand (const cols) (\p i -> p * cols + i) vec :: Acc (Vector Int)

    vecSize = A.length vec
    ancs = gather exp (A.flatten mat)

ancMat'
  :: Acc (Vector Int)
  -> Acc (Matrix Int)
  -> Acc (Matrix Int)
ancMat' = selectRows'

res 
  :: Char
  -> Acc (Vector Int)
  -> Acc (Vector Char)
  -> Acc (Vector Char)
  -> Acc (Matrix Int)
res ch depths types values = ancMat' anc nameC
  where
    nameC = nameCoords' (lift ch) types (nodeCoords depths)
    anc = ancestors depths types (lift ch)

mat :: Acc (Matrix Int)
mat = A.use (fromList (Z :. 2 :. 5) [0..])

vec :: Acc (Vector Int)
vec = A.use $ fromList (Z :. 3) [0..]

