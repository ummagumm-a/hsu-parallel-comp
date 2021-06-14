module Ops where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Control.Lens as Lens
import Data.List
import Utils

    {-
outerProduct :: Acc (Array sh a) -> (Exp a -> Exp b -> Exp c) -> Acc (Array sh' b) -> Acc (Array sh'' c)
outerProduct arr1 g arr2 = A.map (\x -> (helper g x arr2)) arr1
--  where
--    helper x = A.map (g x) arr2
-}

outerVecProduct 
    :: (Elt a, Elt b, Elt c) 
    => Acc (Vector a) 
    -> (Exp a -> Exp b -> Exp c) 
    -> Acc (Vector b) 
    -> Acc (Matrix c)
outerVecProduct vec1 g vec2 = A.reshape (A.lift $ Z :. nEls1 :. nEls2) (A.zipWith g extVec1 extVec2)
  where
    nEls1 = nEls vec1
    nEls2 = nEls vec2

    extVec1 = extVecAlong nEls2 vec1 
    extVec2 = extVecAcross nEls1 vec2 

findFirstOcc 
  :: (Shape sh, Elt a, A.Eq a)
  => Acc (Array sh a)
  -> Exp a
  -> Maybe Int
findFirstOcc arr el
  = findIndex cond (toList (CPU.run arr))
  where
    cond = expToSimple . (A.== el) . A.constant

-- | Check whether vec1 is a prefix of vec2.
-- Prints error if vec1 and vec2 have different lengths.
--
-- >>> let vec = A.use (fromList (Z :. 5) [0..] :: Vector Int)
-- >>> let vec1 = A.use (fromList (Z :. 5) [0, 1, 2, 0, 0] :: Vector Int)
-- >>> CPU.run $ A.unit $ isPrefixOf' vec vec1
-- Scalar Z [True]
--
-- >>> CPU.run $ A.unit $ isPrefixOf' vec1 vec
-- Scalar Z [False]
isPrefixOf'
  :: Acc (Vector Int)
  -> Acc (Vector Int)
  -> Exp Bool
isPrefixOf' vec1 vec2
  | expToSimple cond = ans
  | otherwise = undefined
  where
    cond = A.length vec1 A.== A.length vec2
    ans = A.and (dFork vec2 equal or' zeroRightArg vec1) A.!! 0
    mismatchErr = error "LENGTH ERROR: Mismatched left and right argument shapes"

    equal = A.zipWith (A.==)
    or' = A.zipWith (A.||)
    zeroRightArg = \_ b -> A.map (A.== 0) b

-- | Given a boolean vector vec, 
-- the function selects rows from mat based on the true elements of vec.
--
-- >>> let vec = A.use (fromList (Z :. 3) [True, False, True] :: Vector Bool)
-- >>> let mat = A.use (fromList (Z :. 3 :. 4) [0..] :: Matrix Int)
-- >>> CPU.run mat
-- Matrix (Z :. 3 :. 4)
--  [ 0, 1,  2,  3,
--    4, 5,  6,  7,
--    8, 9, 10, 11]
-- >>> CPU.run $ selectRows vec mat
-- Matrix (Z :. 2 :. 4) 
--  [ 0, 1,  2,  3,
--    8, 9, 10, 11]
selectRows
  :: (Elt a) 
  => Acc (Vector Bool)
  -> Acc (Matrix a)
  -> Acc (Matrix a)
selectRows vec mat 
  = A.reshape (A.lift $ Z :. nRows :. cols) filtered
  where
    Z :. _ :. cols = unlift (A.shape mat) :: Z :. Exp Int :. Exp Int
    sieve = A.replicate (lift (Z :. All :. cols)) vec
    filtered = compact sieve mat ^. _1
    nRows = A.length filtered `A.div` cols 

-- | Calculate the inner product of two matrices. 
-- Analog to X(f.g)Y APL operator.
--
-- >>> let mat1 = A.use (fromList (Z :. 2 :. 3) [0..] :: Matrix Int)
-- >>> CPU.run mat1
-- Matrix (Z :. 2 :. 3)
--  [ 0, 1, 2,
--    3, 4, 5]
--
-- >>> let mat2 = A.use (fromList (Z :. 3 :. 4) [0..] :: Matrix Int)
-- >>> CPU.run mat2
-- Matrix (Z :. 3 :. 4)
--  [ 0, 1,  2,  3,
--    4, 5,  6,  7,
--    8, 9, 10, 11]
--
-- >>> CPU.run $ innerProduct mat1 mat2 (+) (*)
-- Matrix (Z :. 2 :. 4) 
--  [ 20, 23, 26, 29,
--    56, 68, 80, 92]
innerProduct
  :: (Elt a, Elt b, Elt c)
  => Acc (Matrix a)
  -> (Exp c -> Exp c -> Exp c)
  -> (Exp a -> Exp b -> Exp c)
  -> Acc (Matrix b)
  -> Acc (Matrix c)
innerProduct mat1 f g mat2
  = A.fold1 f $ A.zipWith g extMat1 extMat2
  where
    cond = cols1 A.== rows2

    Z :. rows1 :. cols1 = A.unlift (A.shape mat1) :: Z :. Exp Int :. Exp Int
    Z :. rows2 :. cols2 = A.unlift (A.shape mat2) :: Z :. Exp Int :. Exp Int

    extMat1 = A.replicate (A.lift (Z :. All :. cols2 :. All)) mat1 
    extMat2 = A.replicate (A.lift (Z :. rows1 :. All :. All)) (A.transpose mat2)

-- arr (9x5) . arr (5x2)
-- extend the first to 9x5x2 
-- extend the second to 9x5x2
-- take the penultimate axis
 
-- | A monadic two-train tacit function.
atop 
  :: (Shape sh, Shape sh', Shape sh'',
      Elt a, Elt a', Elt a'')
  => (Acc (Array sh' a') -> Acc (Array sh'' a''))
  -> (Acc (Array sh a) -> Acc (Array sh' a'))
  -> Acc (Array sh a)
  -> Acc (Array sh'' a'')
atop f g = f . g

-- | A monadic three-train tacit function.
fork
  :: (Shape sh, Shape sh1, Shape sh2, Shape sh3,
      Elt a, Elt a1, Elt a2, Elt a3)
  => (Acc (Array sh a) -> Acc (Array sh1 a1))
  -> (Acc (Array sh1 a1) -> Acc (Array sh2 a2) -> Acc (Array sh3 a3))
  -> (Acc (Array sh a) -> Acc (Array sh2 a2))
  -> Acc (Array sh a)
  -> Acc (Array sh3 a3)
fork f g h x = f x `g` h x

-- | A monadic three-train tacit function
-- with a constant as a first argument.
cFork
  :: (Shape sh, Shape sh1, Shape sh2, Shape sh3,
      Elt a, Elt a1, Elt a2, Elt a3)
  => Acc (Array sh1 a1)
  -> (Acc (Array sh1 a1) -> Acc (Array sh2 a2) -> Acc (Array sh3 a3))
  -> (Acc (Array sh a) -> Acc (Array sh2 a2))
  -> Acc (Array sh a)
  -> Acc (Array sh3 a3)
cFork a g h x = a `g` h x

-- | A dyadic two-train tacit function.
dAtop 
  :: (Shape sh, Shape sh1, Shape sh2, Shape sh3,
      Elt a, Elt a1, Elt a2, Elt a3)
  => Acc (Array sh1 a1) 
  -> (Acc (Array sh3 a3) -> Acc (Array sh a))
  -> (Acc (Array sh1 a1) -> Acc (Array sh2 a2) -> Acc (Array sh3 a3))
  -> Acc (Array sh2 a2)
  -> Acc (Array sh a)
dAtop x f g y = f (x `g` y)

-- | A dyadic three-train tacit function.
dFork
  :: (Shape sh, Shape sh1, Shape sh2, Shape sh3, Shape sh4,
      Elt a, Elt a1, Elt a2, Elt a3, Elt a4)
  => Acc (Array sh4 a4)
  -> (Acc (Array sh4 a4) -> Acc (Array sh a) -> Acc (Array sh1 a1))
  -> (Acc (Array sh1 a1) -> Acc (Array sh2 a2) -> Acc (Array sh3 a3))
  -> (Acc (Array sh4 a4) -> Acc (Array sh a) -> Acc (Array sh2 a2))
  -> Acc (Array sh a)
  -> Acc (Array sh3 a3)
dFork x f g h y = (x `f` y) `g` (x `h` y) 

-- | A dyadic three-train tacit function
-- with a constant as a first argument.
dcFork
  :: (Shape sh, Shape sh1, Shape sh2, Shape sh3,
      Elt a, Elt a1, Elt a2, Elt a3)
  => Acc (Array sh1 a1)
  -> Acc (Array sh4 a4)
  -> (Acc (Array sh4 a4) -> Acc (Array sh2 a2) -> Acc (Array sh3 a3))
  -> (Acc (Array sh1 a1) -> Acc (Array sh a) -> Acc (Array sh2 a2))
  -> Acc (Array sh a)
  -> Acc (Array sh3 a3)
dcFork x a g h y = a `g` (x `h` y)

    {-
train3 
  :: (Shape sh, Elt a)
  => Acc (Array sh a)
 -} 

-- | Applies g to corresponding elements of vec and rows of mat
-- let vec = fromList (Z :. 3) [0..] :: Vector Int
-- Vector (Z :. 3) [0,1,2]
-- 
-- let mat = fromList (Z :. 3 :. 3) [0..] :: Matrix Int
-- Matrix (Z :. 3 :. 3) 
--  [ 0, 1, 2,
--    3, 4, 5,
--    6, 7, 8]
--
-- CPU.run $ elRowOp (A.use vec) (A.+) (A.use mat)
-- Matrix (Z :. 3 :. 3) 
--  [ 0, 1,  2,
--    4, 5,  6,
--    8, 9, 10]
    {-
elRowOp 
  :: (Elt a, Elt b, Elt c) 
  =>  Acc (Vector a) 
  -> (Exp a -> Acc (Vector b) -> Acc (Vector c)) 
  -> Acc (Matrix b) 
  -> Acc (Matrix c)
elRowOp vec g mat 
  = helper 0 
  where
    (n1,n2) = expToSimple $ A.unindex2 (A.shape mat)
   
    helper k
      | k P.== (n1 - 1) = resVec
      | otherwise = concatOn _1 resVec (helper (k + 1))
        where
          resVec = vecToMat $ g vecEl row
          vecEl = (A.!!) vec (A.lift k)
          row = slice mat (A.constant (Z :. (k :: Int) :. All))
-}
