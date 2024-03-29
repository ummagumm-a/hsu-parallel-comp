module Ops where


import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Control.Lens as Lens
import Data.List
import Utils

-- Operations:
-- Vector -> Matrix -> Scalar
-- Vector -> Matrix -> Vector
-- Vector -> Matrix -> Matrix

-- mat = [  0,  1,  2,  3,
--          4,  5,  6,  7,
--          8,  9, 10, 11,
--          12, 13, 14, 15,
--          16, 17, 18, 19,
--          20, 21, 22, 23,
--          24, 25, 26, 27,
--          28, 29, 30, 31,
--          32, 33, 34, 35]
--
-- keys = [[0, 1]]
--
-- segs = [4]
--
-- f = \_ m -> A.size m
--
-- keyOp [  0,  1,  2,  3,  [[0,1]] [4] (\_ m -> A.size m)
--          4,  5,  6,  7,
--          8,  9, 10, 11,
--          12, 13, 14, 15,
--          16, 17, 18, 19,
--          20, 21, 22, 23,
--          24, 25, 26, 27,
--          28, 29, 30, 31,
--          32, 33, 34, 35]
--
-- segsMod = A.zip (A.scanl (+) 0 [4]) [4]
-- segsMod = A.zip [0,4] [4]
-- segsMod = [(0,4)]
--
-- ans [0,1] [(0,4)]
-- ans = scalToVec (A.unit res)
-- ans = scalToVec (A.unit (f [0,1] subMv)
-- ans = scalToVec (A.unit (f [0,1] (selectRows' inds mv))
-- ans = scalToVec (A.unit (f [0,1] (selectRows' (enumFromN (A.lift $ Z :. length) offset) mv))
-- ans = scalToVec (A.unit (f [0,1] (selectRows' (enumFromN (A.lift $ Z :. 4) 0) mv))
-- ans = scalToVec (A.unit (f [0,1] (selectRows' [0,1,2,3] mv))
-- ans = scalToVec (A.unit (f [0,1] ([ 0, 1, 2, 3,
--                                     4, 5, 6, 7,
--                                     8, 9, 10, 11,
--                                     12, 13, 14, 15 ])
-- ans = [16] :: Vector (Z :. 1)

f 
  :: (Elt e, Elt v)
  => Acc (Vector e)
  -> Acc (Matrix v)
  -> Exp Int
f _ = A.size 

keyOp 
  :: (Elt k, Elt v, Elt r, Eq k)
  => Acc (Matrix k, Matrix v, Segments Int)
  -> (Acc (Vector k) -> Acc (Matrix v) -> Exp r)
  -> Acc (Vector r)
keyOp (T3 mk mv segs) f 
  = helper (tMk mk) (A.tail segsMod) (ans mk segsMod)
  where
    segsMod = A.zip (A.scanl (+) 0 segs) segs

    tMk = A.transpose . A.tail . A.transpose

    ans mk' segs' = scalToVec (A.unit res)
      where
        res = f (slice0 mk') subMv
        
        (T2 offset length) = segs' A.!! 0
        inds = enumFromN (A.lift $ Z :. length) offset
        subMv = selectRows' inds mv

        slice0 mat = flatten $ slice mat (constant (Z :. (0 :: Int) :. All))
 

    helper mk' segs' acc
      = ifThenElse (the (A.compute $ A.unit cond)) acc 
          (helper (tMk mk') (A.tail segs') (acc A.++ vecRes))
      where
        cond = A.null segs'

        vecRes = ans mk' segs'

ans f mk' mv segs' = scalToVec (A.unit res)
      where
        res = f (slice0 mk') subMv
        
        (T2 offset length) = segs' A.!! 0
        inds = enumFromN (A.lift $ Z :. length) offset
        subMv = selectRows' inds mv

        slice0 mat = flatten $ slice mat (constant (Z :. (0 :: Int) :. All))
 

        
groupByKeys :: (Elt k, Elt v, Eq k)
    => Acc (Matrix k)
    -> Acc (Matrix v)
    -> Acc (Matrix k, Matrix v, Segments Int)
groupByKeys keys vals 
  = T3 (selectRows' uniqueRowsIdxVec keys) 
       (selectRows' selectors' vals) 
        descriptor
    where
        (I2 nKeysRows nKeysCols) = shape keys

        -- each row contains indexes of equal rows
        -- groupsMatrix: nKeysRows x nKeysRows
        groupsMatrix = A.imap (\(I2 _ j) v -> boolToInt v * (j + 1) - 1)
            $ innerProduct' (==) (&&) keys keys

        -- if a or b is zero, min a b is zero, and if not, (a == 0 || b == 0) is zero
        chooseMinId :: Exp Int -> Exp Int -> Exp Int
        chooseMinId a b = if a < 0 || b < 0 then max a b else min a b
        uniqueRowsIdxVec = A.afst
                         $ A.filter (>= 0)
                         $ A.imap (\(I1 i) n -> boolToInt (i == n) * (n + 1) - 1)
                         $ fold1 chooseMinId groupsMatrix

        (T2 selectors' descriptor) = A.filter (> 0) $ selectRows' uniqueRowsIdxVec groupsMatrix 

-- | Produces an outer vector product of two vectors
-- with some function. 
--
-- >>> let vec = fromList (Z :. 3) [0..] :: Vector Int
-- >>> vec
-- Vector (Z :. 3) [0,1,2]
--
-- >>> CPU.run $ outerVecProduct vec (A.*) vec
-- Matrix (Z :. 3 :. 3)
--   [ 0, 0, 0,
--     0, 1, 2,
--     0, 2, 4]
outerVecProduct 
    :: (Elt a, Elt b, Elt c) 
    => Acc (Vector a) 
    -> (Exp a -> Exp b -> Exp c) 
    -> Acc (Vector b) 
    -> Acc (Matrix c)
outerVecProduct vec1 g vec2 
  = A.reshape newShape (A.zipWith g extVec1 extVec2)
  where
    nEls1 = A.size vec1
    nEls2 = A.size vec2

    newShape = A.lift $ Z :. nEls1 :. nEls2

    extVec1 = extVecAlong nEls2 vec1 
    extVec2 = extVecAcross nEls1 vec2

-- | Given a boolean vector vec, 
-- the function selects rows from mat 
-- based on the corresponding true elements of vec.
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
  = A.reshape shape' filtered
  where
    (I2 _ cols) = shape mat
    shape' = A.lift $ Z :. nRows :. cols

    sieve = A.replicate (lift (Z :. All :. cols)) vec
    filtered = A.afst $ compact sieve mat
    nRows = A.length filtered `A.div` cols 

selectRows'
  :: (Elt a)
  => Acc (Vector Int)
  -> Acc (Matrix a)
  -> Acc (Matrix a)
selectRows' vec mat 
  = A.reshape (lift (Z :. vecSize :. cols)) ancs
  where
    (I2 rows cols) = shape mat

    exp = A.expand (const cols) (\p i -> p * cols + i) vec :: Acc (Vector Int)

    vecSize = A.length vec
    ancs = gather exp (A.flatten mat)


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

    (I2 rows1 cols1) = shape mat1
    (I2 rows2 cols2) = shape mat2

    extMat1 = A.replicate (A.lift (Z :. All :. cols2 :. All)) mat1 
    extMat2 = A.replicate (A.lift (Z :. rows1 :. All :. All)) (A.transpose mat2)

-- Inner product of 2 matrices (general case of matrix multiplication with custom product and sum combinators)
innerProduct' 
    :: (Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)  -- product function: how to combine 2 elements from two matrices
    -> (Exp c -> Exp c -> Exp c)  -- sum function: how to combine the row of results into single element
    -> Acc (Matrix a)             -- ma x x
    -> Acc (Matrix b)             -- x x nb 
    -> Acc (Matrix c)
innerProduct' prodF sumF a b 
  = fold1 sumF $ A.zipWith prodF aExt bExt
    where
        -- r1 == c2 - precondition
        (I2 r1 _) = shape a
        (I2 r2 _) = shape b

        aExt = A.replicate (lift (Z :. All :. r2 :. All)) a
        bExt = A.replicate (lift (Z :. r1 :. All :. All)) b

-- 2x5 5x9

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
