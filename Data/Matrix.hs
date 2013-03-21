
-- | Matrix datatype an basic operations.
module Data.Matrix (
    -- * Matrix type
    Matrix , prettyMatrix
  , nrows , ncols
    -- * Builders
  , zero
  , identity
  , matrix
  , fromLists
    -- * Accessing
  , getElem , (!)
    -- * Manipulating matrices
  , transpose , extendTo
    -- * Working with blocks
    -- ** Splitting blocks
  , submatrix
  , splitBlocks
    -- ** Joining blocks
  , (<|>) , (<->)
  , joinBlocks
    -- * Matrix multiplication
  , multStd
  , multStrassen
  , multStrassenMixed
  ) where

import Data.Monoid
import Control.DeepSeq
import qualified Data.Vector as V

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX TYPE

data Matrix a = M {
   nrows :: !Int -- ^ Number of rows.
 , ncols :: !Int -- ^ Number of columns.
 , mvect ::  V.Vector a
   } deriving Eq

-- | Just a cool way to output the size of a matrix.
sizeStr :: Int -> Int -> String
sizeStr n m = show n ++ "x" ++ show m

-- | Display a matrix as a 'String'.
prettyMatrix :: Show a => Matrix a -> String
prettyMatrix m@(M _ _ v) = unlines
 [ "( " <> unwords (fmap (\j -> fill mx $ show $ m ! (i,j)) [1..ncols m]) <> " )" | i <- [1..nrows m] ]
 where
  mx = V.maximum $ fmap (length . show) v
  fill k str = replicate (k - length str) ' ' ++ str

instance Show a => Show (Matrix a) where
 show = prettyMatrix

instance NFData a => NFData (Matrix a) where
 rnf (M _ _ v) = rnf v

-------------------------------------------------------
-------------------------------------------------------
---- ENCODING/DECODING

-- Encoding/decoding rules
{-# RULES
"matrix/encode" forall m x. decode m (encode m x) = x
"matrix/decode" forall m x. encode m (decode m x) = x
  #-}

-- | One-dimensional encoding of a two-dimensional index.
--
-- 'decode' m '.' 'encode' m = 'id'
--
encode :: Int -- ^ Columns of the matrix.
       -> (Int,Int) -> Int
{-# INLINE encode #-}
encode m (i,j) = (i-1) * m + j - 1

-- | One-dimensional decoding of a two-dimensional index.
--
-- 'encode' m '.' 'decode' m = 'id'
--
decode :: Int -- ^ Columns of the matrix.
       -> Int -> (Int,Int)
{-# INLINE decode #-}
decode m k = (q+1,r+1)
 where
  (q,r) = quotRem k m

-------------------------------------------------------
-------------------------------------------------------
---- BUILDERS

-- | The zero matrix of the given size.
zero :: Num a =>
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix a
zero n m = M n m $ V.replicate (n*m) 0

-- | Generate a matrix from a generator function.
matrix :: Int -- ^ Rows
       -> Int -- ^ Columns
       -> ((Int,Int) -> a) -- ^ Generator function
       -> Matrix a
matrix n m f = M n m $ V.generate (n*m) (f . decode m)

-- | Identity matrix of the given order.
identity :: Num a => Int -> Matrix a
identity n = matrix n n $ \(i,j) -> if i == j then 1 else 0

fromLists :: [[a]] -> Matrix a
fromLists xss = M (length xss) (length $ head xss) $ mconcat $ fmap V.fromList xss

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | Get an element of a matrix.
getElem :: Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
getElem i j (M n m v)
 | i > n || j > m = error $ "Trying to get the " ++ show (i,j) ++ " element from a "
                         ++ sizeStr n m ++ " matrix."
 | otherwise = v V.! encode m (i,j)

-- | Nice alias for 'getElem'.
(!) :: Matrix a -> (Int,Int) -> a
m ! (i,j) = getElem i j m

-------------------------------------------------------
-------------------------------------------------------
---- MANIPULATING MATRICES

-- | The transpose of a matrix.
transpose :: Matrix a -> Matrix a
transpose (M n m v) = M m n $ V.backpermute v $
 fmap (\k -> let (q,r) = quotRem k n
             in  r*m + q
       ) $ V.enumFromN 0 (V.length v)

-- | Extend a matrix to a given size adding zeroes.
--   If the matrix already has the required size, nothing happens.
extendTo :: Num a
         => Int -- ^ Minimal number of rows.
         -> Int -- ^ Minimal number of columns.
         -> Matrix a -> Matrix a
extendTo n m a = a''
 where
  n'  = n - nrows a
  a'  = if n' <= 0 then a  else a  <-> zero n' (ncols a)
  m'  = m - ncols a
  a'' = if m' <= 0 then a' else a' <|> zero (nrows a') m'

-------------------------------------------------------
-------------------------------------------------------
---- WORKING WITH BLOCKS

-- | Extract a submatrix.
submatrix :: Int    -- ^ Starting row
             -> Int -- ^ Ending row
          -> Int    -- ^ Starting column
             -> Int -- ^ Ending column
          -> Matrix a
          -> Matrix a
submatrix r1 r2 c1 c2 (M _ m v) = M (r2-r1+1) m' $ V.force $
 mconcat [ V.unsafeSlice (encode m (r,c1)) m' v | r <- [r1 .. r2] ]
  where
   m' = c2-c1+1

-- | Make a block-partition of a matrix using a given element as reference.
--   The element will stay in the bottom-right corner of the top-left corner matrix.
--
-- >                 (             )   (      |      )
-- >                 (             )   ( ...  | ...  )
-- >                 (    x        )   (    x |      )
-- > splitBlocks i j (             ) = (-------------) , where x = a_{i,j}
-- >                 (             )   (      |      )
-- >                 (             )   ( ...  | ...  )
-- >                 (             )   (      |      )
--
--   Note that some blocks can end up empty. We use the following notation for these blocks:
--
-- > ( TL | TR )
-- > (---------)
-- > ( BL | BR )
--
--   Where T = Top, B = Bottom, L = Left, R = Right.
--
--   Implementation is done via slicing of vectors.
splitBlocks :: Int      -- ^ Row of the splitting element.
            -> Int      -- ^ Column of the splitting element.
            -> Matrix a -- ^ Matrix to split.
            -> (Matrix a,Matrix a
               ,Matrix a,Matrix a) -- ^ (TL,TR,BL,BR)
splitBlocks i j a@(M n m _) = ( submatrix    1  i 1 j a , submatrix    1  i (j+1) m a
                              , submatrix (i+1) n 1 j a , submatrix (i+1) n (j+1) m a )

-- | Join blocks of the form detailed in 'splitBlocks'.
joinBlocks :: (Matrix a,Matrix a
              ,Matrix a,Matrix a)
           ->  Matrix a
joinBlocks (tl,tr,bl,br) = (tl <|> tr)
                               <->
                           (bl <|> br)

-- | Horizontally join two matrices. Visually:
--
-- > ( A ) <|> ( B ) = ( A | B )
--
-- Where both matrices /A/ and /B/ have the same number of rows.
(<|>) :: Matrix a -> Matrix a -> Matrix a
(M n m v) <|> (M n' m' v')
 | n /= n' = error $ "Horizontal join of " ++ sizeStr n m ++ " and "
                  ++ sizeStr n' m' ++ " matrices."
 | otherwise = let v'' = mconcat [ V.slice (encode m  (r,1)) m  v
                                <> V.slice (encode m' (r,1)) m' v'
                                    | r <- [1..n] ]
               in  M n (m+m') v''

-- | Vertically join two matrices. Visually:
--
-- >                   ( A )
-- > ( A ) <-> ( B ) = ( - )
-- >                   ( B )
--
-- Where both matrices /A/ and /B/ have the same number of columns.
(<->) :: Matrix a -> Matrix a -> Matrix a
(M n m v) <-> (M n' m' v')
 | m /= m' = error $ "Vertical join of " ++ sizeStr n m ++ " and "
                  ++ sizeStr n' m' ++ " matrices."
 | otherwise = M (n+n') m $ v <> v'

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX MULTIPLICATION

-- | Standard matrix multiplication by definition.
multStd :: Num a => Matrix a -> Matrix a -> Matrix a
multStd a1@(M n m _) a2@(M n' m' _)
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise = matrix n m' $ \(i,j) -> sum [ a1 ! (i,k) * a2 ! (k,j) | k <- [1 .. m] ]

first :: (a -> Bool) -> [a] -> a
first f = go
 where
  go (x:xs) = if f x then x else go xs
  go [] = error "first: no element match the condition."

-- | Strassen's algorithm over square matrices of order @2^n@.
strassen :: Num a => Matrix a -> Matrix a -> Matrix a
-- Trivial 1x1 multiplication.
strassen (M 1 1 v) (M 1  1  v') = M 1 1 $ V.zipWith (*) v v'
-- General case guesses that the input matrices are square matrices
-- whose order is a power of two.
strassen a b = joinBlocks (c11,c12,c21,c22)
 where
  -- Size of the subproblem is halved.
  n = div (nrows a) 2
  -- Split of the original problem into smaller subproblems.
  (a11,a12,a21,a22) = splitBlocks n n a
  (b11,b12,b21,b22) = splitBlocks n n b
  -- The seven Strassen's products.
  p1 = strassen (a11 + a22) (b11 + b22)
  p2 = strassen (a21 + a22)  b11
  p3 = strassen  a11        (b12 - b22)
  p4 = strassen        a22  (b21 - b11)
  p5 = strassen (a11 + a12)        b22
  p6 = strassen (a21 - a11) (b11 + b12)
  p7 = strassen (a12 - a22) (b21 + b22)
  -- Merging blocks
  c11 = p1 + p4 - p5 + p7
  c12 = p3 + p5
  c21 = p2 + p4
  c22 = p1 - p2 + p3 + p6

-- | Strassen's matrix multiplication.
multStrassen :: Num a => Matrix a -> Matrix a -> Matrix a
multStrassen a1@(M n m _) a2@(M n' m' _)
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   -- Otherwise, Strassen's Subcubic Matrix Multiplication Algorithm.
   | otherwise =
       let mx = maximum [n,m,n',m']
           n2  = first (>= mx) $ fmap (2^) [(0 :: Int)..]
           b1 = extendTo n2 n2 a1
           b2 = extendTo n2 n2 a2
       in  submatrix 1 n 1 m' $ strassen b1 b2

strmixFactor :: Int
strmixFactor = 2^5 + 1

-- | Strassen's algorithm over square matrices of order @2^n@.
strassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
-- Trivial 1x1 multiplication.
strassenMixed m1@(M n _ _) m2 | n < strmixFactor = multStd m1 m2
-- General case guesses that the input matrices are square matrices
-- whose order is a power of two.
strassenMixed a b = joinBlocks (c11,c12,c21,c22)
 where
  -- Size of the subproblem is halved.
  n = div (nrows a) 2
  -- Split of the original problem into smaller subproblems.
  (a11,a12,a21,a22) = splitBlocks n n a
  (b11,b12,b21,b22) = splitBlocks n n b
  -- The seven Strassen's products.
  p1 = strassenMixed (a11 + a22) (b11 + b22)
  p2 = strassenMixed (a21 + a22)  b11
  p3 = strassenMixed  a11        (b12 - b22)
  p4 = strassenMixed        a22  (b21 - b11)
  p5 = strassenMixed (a11 + a12)        b22
  p6 = strassenMixed (a21 - a11) (b11 + b12)
  p7 = strassenMixed (a12 - a22) (b21 + b22)
  -- Merging blocks
  c11 = p1 + p4 - p5 + p7
  c12 = p3 + p5
  c21 = p2 + p4
  c22 = p1 - p2 + p3 + p6

-- | Mixed Strassen's matrix multiplication.
multStrassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
multStrassenMixed a1@(M n m _) a2@(M n' m' _)
   | n < strmixFactor = multStd a1 a2
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   -- Otherwise, Strassen's Subcubic Matrix Multiplication Algorithm.
   | otherwise =
       let mx = maximum [n,m,n',m']
           n2  = first (>= mx) $ fmap (2^) [(0 :: Int)..]
           b1 = extendTo n2 n2 a1
           b2 = extendTo n2 n2 a2
       in  submatrix 1 n 1 m' $ strassenMixed b1 b2

-------------------------------------------------------
-------------------------------------------------------
---- FUNCTOR INSTANCE

instance Functor Matrix where
 fmap f (M n m v) = M n m $ fmap f v

-------------------------------------------------------
-------------------------------------------------------
---- NUMERICAL INSTANCE

instance Num a => Num (Matrix a) where
 fromInteger = M 1 1 . V.singleton . fromInteger
 negate = fmap negate
 abs = fmap abs
 signum = fmap signum
 -- Addition of matrices.
 (M n m v) + (M n' m' v')
   -- Checking that sizes match...
   | n /= n' || m /= m' = error $ "Addition of " ++ sizeStr n m ++ " and "
                               ++ sizeStr n' m' ++ " matrices."
   -- Otherwise, trivial zip.
   | otherwise = M n m $ V.zipWith (+) v v'
 -- Multiplication of matrices.
 (*) = multStrassenMixed