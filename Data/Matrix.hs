
-- | Matrix datatype and operations.
--
--   Every provided example has been tested.
--   Run @cabal test@ for further tests.
module Data.Matrix (
    -- * Matrix type
    Matrix , prettyMatrix
  , nrows , ncols
  , forceMatrix
    -- * Builders
  , matrix
  , rowVector
  , colVector
    -- ** Special matrices
  , zero
  , identity
  , permMatrix
    -- * List conversions
  , fromList , fromLists
  , toList   , toLists
    -- * Accessing
  , getElem , (!) , unsafeGet , safeGet
  , getRow  , getCol
  , getDiag
  , getMatrixAsVector
    -- * Manipulating matrices
  , setElem
  , unsafeSet
  , transpose , setSize , extendTo
  , mapRow , mapCol
    -- * Submatrices
    -- ** Splitting blocks
  , submatrix
  , minorMatrix
  , splitBlocks
   -- ** Joining blocks
  , (<|>) , (<->)
  , joinBlocks
    -- * Matrix operations
  , elementwise, elementwiseUnsafe
    -- * Matrix multiplication
    -- ** About matrix multiplication
    -- $mult

    -- ** Functions
  , multStd
  , multStd2
  , multStrassen
  , multStrassenMixed
    -- * Linear transformations
  , scaleMatrix
  , scaleRow
  , combineRows
  , switchRows
  , switchCols
    -- * Decompositions
  , luDecomp , luDecompUnsafe
  , luDecomp', luDecompUnsafe'
  , cholDecomp
    -- * Properties
  , trace , diagProd
    -- ** Determinants
  , detLaplace
  , detLU
  , flatten
  ) where

-- Classes
import Control.DeepSeq
import Control.Monad (forM_)
import Control.Loop (numLoop,numLoopFold)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Data.Traversable()
-- Data
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.List               (maximumBy,foldl1')
import           Data.Ord                (comparing)
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
import Data.Maybe

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX TYPE

encode :: Int -> (Int,Int) -> Int
{-# INLINE encode #-}
encode m (i,j) = (i-1)*m + j - 1

decode :: Int -> Int -> (Int,Int)
{-# INLINE decode #-}
decode m k = (q+1,r+1)
 where
  (q,r) = quotRem k m

-- | Type of matrices.
--
--   Elements can be of any type. Rows and columns
--   are indexed starting by 1. This means that, if @m :: Matrix a@ and
--   @i,j :: Int@, then @m ! (i,j)@ is the element in the @i@-th row and
--   @j@-th column of @m@.
data Matrix a = M {
   nrows     :: {-# UNPACK #-} !Int -- ^ Number of rows.
 , ncols     :: {-# UNPACK #-} !Int -- ^ Number of columns.
 , rowOffset :: {-# UNPACK #-} !Int
 , colOffset :: {-# UNPACK #-} !Int
 , vcols     :: {-# UNPACK #-} !Int -- ^ Number of columns of the matrix without offset
 , mvect     :: V.Vector a          -- ^ Content of the matrix as a plain vector.
   }

instance Eq a => Eq (Matrix a) where
  m1 == m2 =
    let r = nrows m1
        c = ncols m1
    in  and $ (r == nrows m2) : (c == ncols m2)
            : [ m1 ! (i,j) == m2 ! (i,j) | i <- [1 .. r] , j <- [1 .. c] ]

-- | Just a cool way to output the size of a matrix.
sizeStr :: Int -> Int -> String
sizeStr n m = show n ++ "x" ++ show m

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: Show a => Matrix a -> String
prettyMatrix m@(M _ _ _ _ _ v) = unlines
 [ "( " <> unwords (fmap (\j -> fill mx $ show $ m ! (i,j)) [1..ncols m]) <> " )" | i <- [1..nrows m] ]
 where
  mx = V.maximum $ fmap (length . show) v
  fill k str = replicate (k - length str) ' ' ++ str

instance Show a => Show (Matrix a) where
 show = prettyMatrix

instance NFData a => NFData (Matrix a) where
 rnf = rnf . mvect

-- | /O(rows*cols)/. Similar to 'V.force'. It copies the matrix content
--   dropping any extra memory.
--
--   Useful when using 'submatrix' from a big matrix.
--
forceMatrix :: Matrix a -> Matrix a
forceMatrix m = matrix (nrows m) (ncols m) $ \(i,j) -> unsafeGet i j m

-------------------------------------------------------
-------------------------------------------------------
---- FUNCTOR INSTANCE

instance Functor Matrix where
 {-# INLINE fmap #-}
 fmap f (M n m ro co w v) = M n m ro co w $ V.map f v

-------------------------------------------------------
-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------
---- MONOID INSTANCE

instance Monoid a => Monoid (Matrix a) where
  mempty = fromList 1 1 [mempty] 
  mappend m m' = matrix (max (nrows m) (nrows m')) (max (ncols m) (ncols m')) $ uncurry zipTogether
    where zipTogether row column = fromMaybe mempty $ safeGet row column m <> safeGet row column m'


-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------
---- APPLICATIVE INSTANCE
---- Works like tensor product but applies a function 

instance Applicative Matrix where
  pure x = fromList 1 1 [x] 
  m <*> m' = flatten $ ((\f -> f <$> m') <$> m)


-------------------------------------------------------
-------------------------------------------------------



-- | Flatten a matrix of matrices. All sub matrices must have same dimensions
--   This criteria is not checked. 
flatten:: (Matrix (Matrix a)) -> Matrix a
flatten m = foldl1 (<->) $ map (foldl1 (<|>) . (\i -> getRow i m)) [1..(nrows m)]

-- | /O(rows*cols)/. Map a function over a row.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 2 3 )
-- >                          ( 4 5 6 )   ( 5 6 7 )
-- > mapRow (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
--
mapRow :: (Int -> a -> a) -- ^ Function takes the current column as additional argument.
        -> Int            -- ^ Row to map.
        -> Matrix a -> Matrix a
mapRow f r m =
  matrix (nrows m) (ncols m) $ \(i,j) ->
    let a = unsafeGet i j m
    in  if i == r
           then f j a
           else a

-- | /O(rows*cols)/. Map a function over a column.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 3 3 )
-- >                          ( 4 5 6 )   ( 4 6 6 )
-- > mapCol (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 9 9 )
--
mapCol :: (Int -> a -> a) -- ^ Function takes the current row as additional argument.
        -> Int            -- ^ Column to map.
        -> Matrix a -> Matrix a
mapCol f c m =
  matrix (nrows m) (ncols m) $ \(i,j) ->
    let a = unsafeGet i j m
    in  if j == c
           then f i a
           else a

-------------------------------------------------------
-------------------------------------------------------
---- FOLDABLE AND TRAVERSABLE INSTANCES

instance Foldable Matrix where
 foldMap f = foldMap f . mvect . forceMatrix

instance Traversable Matrix where
 sequenceA m = fmap (M (nrows m) (ncols m) 0 0 (ncols m)) . sequenceA . mvect $ forceMatrix m

-------------------------------------------------------
-------------------------------------------------------
---- BUILDERS

-- | /O(rows*cols)/. The zero matrix of the given size.
--
-- > zero n m =
-- >                 m
-- >   1 ( 0 0 ... 0 0 )
-- >   2 ( 0 0 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 0 0 )
-- >   n ( 0 0 ... 0 0 )
zero :: Num a =>
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix a
{-# INLINE zero #-}
zero n m = M n m 0 0 m $ V.replicate (n*m) 0

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix :: Int -- ^ Rows
       -> Int -- ^ Columns
       -> ((Int,Int) -> a) -- ^ Generator function
       -> Matrix a
{-# INLINE matrix #-}
matrix n m f = M n m 0 0 m $ V.create $ do
  v <- MV.new $ n * m
  let en = encode m
  numLoop 1 n $
    \i -> numLoop 1 m $
    \j -> MV.unsafeWrite v (en (i,j)) (f (i,j))
  return v

-- | /O(rows*cols)/. Identity matrix of the given order.
--
-- > identity n =
-- >                 n
-- >   1 ( 1 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 1 0 )
-- >   n ( 0 0 ... 0 1 )
--
identity :: Num a => Int -> Matrix a
identity n = matrix n n $ \(i,j) -> if i == j then 1 else 0

-- | Create a matrix from a non-empty list given the desired size.
--   The list must have at least /rows*cols/ elements.
--   An example:
--
-- >                       ( 1 2 3 )
-- >                       ( 4 5 6 )
-- > fromList 3 3 [1..] =  ( 7 8 9 )
--
fromList :: Int -- ^ Rows
         -> Int -- ^ Columns
         -> [a] -- ^ List of elements
         -> Matrix a
{-# INLINE fromList #-}
fromList n m = M n m 0 0 m . V.fromListN (n*m)

-- | Get the elements of a matrix stored in a list.
--
-- >        ( 1 2 3 )
-- >        ( 4 5 6 )
-- > toList ( 7 8 9 ) = [1,2,3,4,5,6,7,8,9]
--
toList :: Matrix a -> [a]
toList m = [ unsafeGet i j m | i <- [1 .. nrows m] , j <- [1 .. ncols m] ]

-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
--
-- >         ( 1 2 3 )   [ [1,2,3]
-- >         ( 4 5 6 )   , [4,5,6]
-- > toLists ( 7 8 9 ) = , [7,8,9] ]
--
toLists :: Matrix a -> [[a]]
toLists m = [ [ unsafeGet i j m | j <- [1 .. ncols m] ] | i <- [1 .. nrows m] ]

-- | Create a matrix from a non-empty list of non-empty lists.
--   /Each list must have at least as many elements as the first list/.
--   Examples:
--
-- > fromLists [ [1,2,3]      ( 1 2 3 )
-- >           , [4,5,6]      ( 4 5 6 )
-- >           , [7,8,9] ] =  ( 7 8 9 )
--
-- > fromLists [ [1,2,3  ]     ( 1 2 3 )
-- >           , [4,5,6,7]     ( 4 5 6 )
-- >           , [8,9,0  ] ] = ( 8 9 0 )
--
fromLists :: [[a]] -> Matrix a
{-# INLINE fromLists #-}
fromLists [] = error "fromLists: empty list."
fromLists (xs:xss) = fromList n m $ concat $ xs : fmap (take m) xss
  where
    n = 1 + length xss
    m = length xs

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: V.Vector a -> Matrix a
rowVector v = M 1 m 0 0 m v
  where
    m = V.length v

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: V.Vector a -> Matrix a
colVector v = M (V.length v) 1 0 0 1 v

-- | /O(rows*cols)/. Permutation matrix.
--
-- > permMatrix n i j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- When @i == j@ it reduces to 'identity' @n@.
--
permMatrix :: Num a
           => Int -- ^ Size of the matrix.
           -> Int -- ^ Permuted row 1.
           -> Int -- ^ Permuted row 2.
           -> Matrix a -- ^ Permutation matrix.
permMatrix n r1 r2 | r1 == r2 = identity n
permMatrix n r1 r2 = matrix n n f
 where
  f (i,j)
   | i == r1 = if j == r2 then 1 else 0
   | i == r2 = if j == r1 then 1 else 0
   | i == j = 1
   | otherwise = 0

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.
getElem :: Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
{-# INLINE getElem #-}
getElem i j m =
  case safeGet i j m of
    Just x -> x
    Nothing -> error
      $ "getElem: Trying to get the "
     ++ show (i,j)
     ++ " element from a "
     ++ sizeStr (nrows m) (ncols m)
     ++ " matrix."

-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
unsafeGet :: Int      -- ^ Row
          -> Int      -- ^ Column
          -> Matrix a -- ^ Matrix
          -> a
{-# INLINE unsafeGet #-}
unsafeGet i j (M _ _ ro co w v) = V.unsafeIndex v $ encode w (i+ro,j+co)

-- | Short alias for 'getElem'.
(!) :: Matrix a -> (Int,Int) -> a
{-# INLINE (!) #-}
m ! (i,j) = getElem i j m

-- | Internal alias for 'unsafeGet'.
(!.) :: Matrix a -> (Int,Int) -> a
{-# INLINE (!.) #-}
m !. (i,j) = unsafeGet i j m

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M n m _ _ _ _)
 | i > n || j > m || i < 1 || j < 1 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | /O(1)/. Get a row of a matrix as a vector.
getRow :: Int -> Matrix a -> V.Vector a
{-# INLINE getRow #-}
getRow i (M _ m ro co w v) = V.slice (w*(i-1+ro) + co) m v

-- | /O(rows)/. Get a column of a matrix as a vector.
getCol :: Int -> Matrix a -> V.Vector a
{-# INLINE getCol #-}
getCol j (M n _ ro co w v) = V.generate n $ \i -> v V.! encode w (i+1+ro,j+co)

-- | /O(min rows cols)/. Diagonal of a /not necessarily square/ matrix.
getDiag :: Matrix a -> V.Vector a
getDiag m = V.generate k $ \i -> m ! (i+1,i+1)
 where
  k = min (nrows m) (ncols m)

-- | /O(rows*cols)/. Transform a 'Matrix' to a 'V.Vector' of size /rows*cols/.
--  This is equivalent to get all the rows of the matrix using 'getRow'
--  and then append them, but far more efficient.
getMatrixAsVector :: Matrix a -> V.Vector a
getMatrixAsVector = mvect . forceMatrix

-------------------------------------------------------
-------------------------------------------------------
---- MANIPULATING MATRICES

msetElem :: PrimMonad m
         => a -- ^ New element
         -> Int -- ^ Number of columns of the matrix
         -> Int -- ^ Row offset
         -> Int -- ^ Column offset
         -> (Int,Int) -- ^ Position to set the new element
         -> MV.MVector (PrimState m) a -- ^ Mutable vector
         -> m ()
{-# INLINE msetElem #-}
msetElem x w ro co (i,j) v = MV.write v (encode w (i+ro,j+co)) x

unsafeMset :: PrimMonad m
         => a -- ^ New element
         -> Int -- ^ Number of columns of the matrix
         -> Int -- ^ Row offset
         -> Int -- ^ Column offset
         -> (Int,Int) -- ^ Position to set the new element
         -> MV.MVector (PrimState m) a -- ^ Mutable vector
         -> m ()
{-# INLINE unsafeMset #-}
unsafeMset x w ro co (i,j) v = MV.unsafeWrite v (encode w (i+ro,j+co)) x

-- | Replace the value of a cell in a matrix.
setElem :: a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix a -- ^ Original matrix.
        -> Matrix a -- ^ Matrix with the given position replaced with the given value.
{-# INLINE setElem #-}
setElem x p (M n m ro co w v) = M n m ro co w $ V.modify (msetElem x w ro co p) v

-- | Unsafe variant of 'setElem', without bounds checking.
unsafeSet :: a -- ^ New value.
        -> (Int,Int) -- ^ Position to replace.
        -> Matrix a -- ^ Original matrix.
        -> Matrix a -- ^ Matrix with the given position replaced with the given value.
{-# INLINE unsafeSet #-}
unsafeSet x p (M n m ro co w v) = M n m ro co w $ V.modify (unsafeMset x w ro co p) v

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: Matrix a -> Matrix a
transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i)

-- | Extend a matrix to a given size adding a default element.
--   If the matrix already has the required size, nothing happens.
--   The matrix is /never/ reduced in size.
--   Example:
--
-- >                            ( 1 2 3 0 0 )
-- >                ( 1 2 3 )   ( 4 5 6 0 0 )
-- >                ( 4 5 6 )   ( 7 8 9 0 0 )
-- > extendTo 0 4 5 ( 7 8 9 ) = ( 0 0 0 0 0 )
--
-- The definition of 'extendTo' is based on 'setSize':
--
-- > extendTo e n m a = setSize e (max n $ nrows a) (max m $ ncols a) a
--
extendTo :: a   -- ^ Element to add when extending.
         -> Int -- ^ Minimal number of rows.
         -> Int -- ^ Minimal number of columns.
         -> Matrix a -> Matrix a
extendTo e n m a = setSize e (max n $ nrows a) (max m $ ncols a) a

-- | Set the size of a matrix to given parameters. Use a default element
--   for undefined entries if the matrix has been extended.
setSize :: a   -- ^ Default element.
        -> Int -- ^ Number of rows.
        -> Int -- ^ Number of columns.
        -> Matrix a
        -> Matrix a
{-# INLINE setSize #-}
setSize e n m a@(M n0 m0 _ _ _ _) = matrix n m $ \(i,j) ->
  if i <= n0 && j <= m0
     then unsafeGet i j a
     else e

-------------------------------------------------------
-------------------------------------------------------
---- WORKING WITH BLOCKS

-- | /O(1)/. Extract a submatrix given row and column limits.
--   Example:
--
-- >                   ( 1 2 3 )
-- >                   ( 4 5 6 )   ( 2 3 )
-- > submatrix 1 2 2 3 ( 7 8 9 ) = ( 5 6 )
submatrix :: Int    -- ^ Starting row
          -> Int -- ^ Ending row
          -> Int    -- ^ Starting column
          -> Int -- ^ Ending column
          -> Matrix a
          -> Matrix a
{-# INLINE submatrix #-}
submatrix r1 r2 c1 c2 (M n m ro co w v)
  | r1 < 1  || r1 > n = error $ "submatrix: starting row (" ++ show r1 ++ ") is out of range. Matrix has " ++ show n ++ " rows."
  | c1 < 1  || c1 > m = error $ "submatrix: starting column (" ++ show c1 ++ ") is out of range. Matrix has " ++ show m ++ " columns."
  | r2 < r1 || r2 > n = error $ "submatrix: ending row (" ++ show r2 ++ ") is out of range. Matrix has " ++ show n ++ " rows, and starting row is " ++ show r1 ++ "."
  | c2 < c1 || c2 > m = error $ "submatrix: ending column (" ++ show c2 ++ ") is out of range. Matrix has " ++ show m ++ " columns, and starting column is " ++ show c1 ++ "."
  | otherwise = M (r2-r1+1) (c2-c1+1) (ro+r1-1) (co+c1-1) w v

-- | /O(rows*cols)/. Remove a row and a column from a matrix.
--   Example:
--
-- >                 ( 1 2 3 )
-- >                 ( 4 5 6 )   ( 1 3 )
-- > minorMatrix 2 2 ( 7 8 9 ) = ( 7 9 )
minorMatrix :: Int -- ^ Row @r@ to remove.
            -> Int -- ^ Column @c@ to remove.
            -> Matrix a -- ^ Original matrix.
            -> Matrix a -- ^ Matrix with row @r@ and column @c@ removed.
minorMatrix r0 c0 (M n m ro co w v) =
  let r = r0 + ro
      c = c0 + co
  in  M (n-1) (m-1) ro co (w-1) $ V.ifilter (\k _ -> let (i,j) = decode w k in i /= r && j /= c) v

-- | /O(1)/. Make a block-partition of a matrix using a given element as reference.
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
splitBlocks :: Int      -- ^ Row of the splitting element.
            -> Int      -- ^ Column of the splitting element.
            -> Matrix a -- ^ Matrix to split.
            -> (Matrix a,Matrix a
               ,Matrix a,Matrix a) -- ^ (TL,TR,BL,BR)
{-# INLINE[1] splitBlocks #-}
splitBlocks i j a@(M n m _ _ _ _) =
    ( submatrix    1  i 1 j a , submatrix    1  i (j+1) m a
    , submatrix (i+1) n 1 j a , submatrix (i+1) n (j+1) m a )

-- | Join blocks of the form detailed in 'splitBlocks'. Precisely:
--
-- > joinBlocks (tl,tr,bl,br) =
-- >   (tl <|> tr)
-- >       <->
-- >   (bl <|> br)
joinBlocks :: (Matrix a,Matrix a,Matrix a,Matrix a) -> Matrix a
{-# INLINE[1] joinBlocks #-}
joinBlocks (tl,tr,bl,br) =
  let n  = nrows tl
      nb = nrows bl
      n' = n + nb
      m  = ncols tl
      mr = ncols tr
      m' = m + mr
      en = encode m'
  in  M n' m' 0 0 m' $ V.create $ do
        v <- MV.new (n'*m')
        let wr = MV.write v
        numLoop 1 n  $ \i -> do
          numLoop 1 m  $ \j -> wr (en (i ,j  )) $ tl ! (i,j)
          numLoop 1 mr $ \j -> wr (en (i ,j+m)) $ tr ! (i,j)
        numLoop 1 nb $ \i -> do
          let i' = i+n
          numLoop 1 m  $ \j -> wr (en (i',j  )) $ bl ! (i,j)
          numLoop 1 mr $ \j -> wr (en (i',j+m)) $ br ! (i,j)
        return v

{-# RULES
"matrix/splitAndJoin"
   forall i j m. joinBlocks (splitBlocks i j m) = m
  #-}

-- | Horizontally join two matrices. Visually:
--
-- > ( A ) <|> ( B ) = ( A | B )
--
-- Where both matrices /A/ and /B/ have the same number of rows.
-- /This condition is not checked/.
(<|>) :: Matrix a -> Matrix a -> Matrix a
{-# INLINE (<|>) #-}
m <|> m' =
  let c = ncols m
  in  matrix (nrows m) (c + ncols m') $ \(i,j) ->
        if j <= c then m ! (i,j) else m' ! (i,j-c)

-- | Vertically join two matrices. Visually:
--
-- >                   ( A )
-- > ( A ) <-> ( B ) = ( - )
-- >                   ( B )
--
-- Where both matrices /A/ and /B/ have the same number of columns.
-- /This condition is not checked/.
(<->) :: Matrix a -> Matrix a -> Matrix a
{-# INLINE (<->) #-}
m <-> m' =
  let r = nrows m
  in  matrix (r + nrows m') (ncols m) $ \(i,j) ->
        if i <= r then m ! (i,j) else m' ! (i-r,j)

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX OPERATIONS

-- | Perform an operation element-wise.
--   The second matrix must have at least as many rows
--   and columns as the first matrix. If it's bigger,
--   the leftover items will be ignored.
--   If it's smaller, it will cause a run-time error.
--   You may want to use 'elementwiseUnsafe' if you
--   are definitely sure that a run-time error won't
--   arise.
elementwise :: (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
elementwise f m m' = matrix (nrows m) (ncols m) $
  \k -> f (m ! k) (m' ! k)

-- | Unsafe version of 'elementwise', but faster.
elementwiseUnsafe :: (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
{-# INLINE elementwiseUnsafe #-}
elementwiseUnsafe f m m' = matrix (nrows m) (ncols m) $
  \(i,j) -> f (unsafeGet i j m) (unsafeGet i j m')

infixl 6 +., -.

-- | Internal unsafe addition.
(+.) :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE (+.) #-}
(+.) = elementwiseUnsafe (+)

-- | Internal unsafe substraction.
(-.) :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE (-.) #-}
(-.) = elementwiseUnsafe (-)

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX MULTIPLICATION

{- $mult

Four methods are provided for matrix multiplication.

* 'multStd':
     Matrix multiplication following directly the definition.
     This is the best choice when you know for sure that your
     matrices are small.

* 'multStd2':
     Matrix multiplication following directly the definition.
     However, using a different definition from 'multStd'.
     According to our benchmarks with this version, 'multStd2' is
     around 3 times faster than 'multStd'.

* 'multStrassen':
     Matrix multiplication following the Strassen's algorithm.
     Complexity grows slower but also some work is added
     partitioning the matrix. Also, it only works on square
     matrices of order @2^n@, so if this condition is not
     met, it is zero-padded until this is accomplished.
     Therefore, its use is not recommended.

* 'multStrassenMixed':
     This function mixes the previous methods.
     It provides a better performance in general. Method @(@'*'@)@
     of the 'Num' class uses this function because it gives the best
     average performance. However, if you know for sure that your matrices are
     small (size less than 500x500), you should use 'multStd' or 'multStd2' instead,
     since 'multStrassenMixed' is going to switch to those functions anyway.

We keep researching how to get better performance for matrix multiplication.
If you want to be on the safe side, use ('*').

-}

-- | Standard matrix multiplication by definition.
multStd :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd #-}
multStd a1@(M n m _ _ _ _) a2@(M n' m' _ _ _ _)
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise = multStd_ a1 a2

-- | Standard matrix multiplication by definition.
multStd2 :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd2 #-}
multStd2 a1@(M n m _ _ _ _) a2@(M n' m' _ _ _ _)
   -- Checking that sizes match...
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise = multStd__ a1 a2

-- | Standard matrix multiplication by definition, without checking if sizes match.
multStd_ :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd_ #-}
multStd_ a@(M 1 1 _ _ _ _) b@(M 1 1 _ _ _ _) = M 1 1 0 0 1 $ V.singleton $ (a ! (1,1)) * (b ! (1,1))
multStd_ a@(M 2 2 _ _ _ _) b@(M 2 2 _ _ _ _) =
  M 2 2 0 0 2 $
    let -- A
        a11 = a !. (1,1) ; a12 = a !. (1,2)
        a21 = a !. (2,1) ; a22 = a !. (2,2)
        -- B
        b11 = b !. (1,1) ; b12 = b !. (1,2)
        b21 = b !. (2,1) ; b22 = b !. (2,2)
    in V.fromList 
         [ a11*b11 + a12*b21 , a11*b12 + a12*b22
         , a21*b11 + a22*b21 , a21*b12 + a22*b22
           ]
multStd_ a@(M 3 3 _ _ _ _) b@(M 3 3 _ _ _ _) =
  M 3 3 0 0 3 $
    let -- A
        a11 = a !. (1,1) ; a12 = a !. (1,2) ; a13 = a !. (1,3)
        a21 = a !. (2,1) ; a22 = a !. (2,2) ; a23 = a !. (2,3)
        a31 = a !. (3,1) ; a32 = a !. (3,2) ; a33 = a !. (3,3)
        -- B
        b11 = b !. (1,1) ; b12 = b !. (1,2) ; b13 = b !. (1,3)
        b21 = b !. (2,1) ; b22 = b !. (2,2) ; b23 = b !. (2,3)
        b31 = b !. (3,1) ; b32 = b !. (3,2) ; b33 = b !. (3,3)
    in V.fromList
         [ a11*b11 + a12*b21 + a13*b31 , a11*b12 + a12*b22 + a13*b32 , a11*b13 + a12*b23 + a13*b33
         , a21*b11 + a22*b21 + a23*b31 , a21*b12 + a22*b22 + a23*b32 , a21*b13 + a22*b23 + a23*b33
         , a31*b11 + a32*b21 + a33*b31 , a31*b12 + a32*b22 + a33*b32 , a31*b13 + a32*b23 + a33*b33
           ]
multStd_ a@(M n m _ _ _ _) b@(M _ m' _ _ _ _) = matrix n m' $ \(i,j) -> sum [ a !. (i,k) * b !. (k,j) | k <- [1 .. m] ]

multStd__ :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStd__ #-}
multStd__ a b = matrix r c $ \(i,j) -> dotProduct (V.unsafeIndex avs $ i - 1) (V.unsafeIndex bvs $ j - 1)
  where
    r = nrows a
    avs = V.generate r $ \i -> getRow (i+1) a
    c = ncols b
    bvs = V.generate c $ \i -> getCol (i+1) b

dotProduct :: Num a => V.Vector a -> V.Vector a -> a
{-# INLINE dotProduct #-}
dotProduct v1 v2 = numLoopFold 0 (V.length v1 - 1) 0 $
  \r i -> V.unsafeIndex v1 i * V.unsafeIndex v2 i + r

{-
dotProduct v1 v2 = go (V.length v1 - 1) 0
  where
    go (-1) a = a
    go i a = go (i-1) $ (V.unsafeIndex v1 i) * (V.unsafeIndex v2 i) + a
-}

first :: (a -> Bool) -> [a] -> a
first f = go
 where
  go (x:xs) = if f x then x else go xs
  go _ = error "first: no element match the condition."

-- | Strassen's algorithm over square matrices of order @2^n@.
strassen :: Num a => Matrix a -> Matrix a -> Matrix a
-- Trivial 1x1 multiplication.
strassen a@(M 1 1 _ _ _ _) b@(M 1 1 _ _ _ _) = M 1 1 0 0 1 $ V.singleton $ (a ! (1,1)) * (b ! (1,1))
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
multStrassen a1@(M n m _ _ _ _) a2@(M n' m' _ _ _ _)
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | otherwise =
       let mx = maximum [n,m,n',m']
           n2  = first (>= mx) $ fmap (2^) [(0 :: Int)..]
           b1 = setSize 0 n2 n2 a1
           b2 = setSize 0 n2 n2 a2
       in  submatrix 1 n 1 m' $ strassen b1 b2

strmixFactor :: Int
strmixFactor = 300

-- | Strassen's mixed algorithm.
strassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
{-# SPECIALIZE strassenMixed :: Matrix Double -> Matrix Double -> Matrix Double #-}
{-# SPECIALIZE strassenMixed :: Matrix Int -> Matrix Int -> Matrix Int #-}
{-# SPECIALIZE strassenMixed :: Matrix Rational -> Matrix Rational -> Matrix Rational #-}
strassenMixed a b
 | r < strmixFactor = multStd__ a b
 | odd r = let r' = r + 1
               a' = setSize 0 r' r' a
               b' = setSize 0 r' r' b
           in  submatrix 1 r 1 r $ strassenMixed a' b'
 | otherwise =
      M r r 0 0 r $ V.create $ do
         v <- MV.unsafeNew (r*r)
         let en = encode r
             n' = n + 1
         -- c11 = p1 + p4 - p5 + p7
         sequence_ [ MV.write v k $
                         unsafeGet i j p1
                       + unsafeGet i j p4
                       - unsafeGet i j p5
                       + unsafeGet i j p7
                   | i <- [1..n]
                   , j <- [1..n]
                   , let k = en (i,j)
                     ]
         -- c12 = p3 + p5
         sequence_ [ MV.write v k $
                         unsafeGet i j' p3
                       + unsafeGet i j' p5
                   | i <- [1..n]
                   , j <- [n'..r]
                   , let k = en (i,j)
                   , let j' = j - n
                     ]
         -- c21 = p2 + p4
         sequence_ [ MV.write v k $
                         unsafeGet i' j p2
                       + unsafeGet i' j p4
                   | i <- [n'..r]
                   , j <- [1..n]
                   , let k = en (i,j)
                   , let i' = i - n
                     ]
         -- c22 = p1 - p2 + p3 + p6
         sequence_ [ MV.write v k $
                         unsafeGet i' j' p1
                       - unsafeGet i' j' p2
                       + unsafeGet i' j' p3
                       + unsafeGet i' j' p6
                   | i <- [n'..r]
                   , j <- [n'..r]
                   , let k = en (i,j)
                   , let i' = i - n
                   , let j' = j - n
                     ]
         return v
 where
  r = nrows a
  -- Size of the subproblem is halved.
  n = quot r 2
  -- Split of the original problem into smaller subproblems.
  (a11,a12,a21,a22) = splitBlocks n n a
  (b11,b12,b21,b22) = splitBlocks n n b
  -- The seven Strassen's products.
  p1 = strassenMixed (a11 +. a22) (b11 +. b22)
  p2 = strassenMixed (a21 +. a22)  b11
  p3 = strassenMixed  a11         (b12 -. b22)
  p4 = strassenMixed         a22  (b21 -. b11)
  p5 = strassenMixed (a11 +. a12)         b22
  p6 = strassenMixed (a21 -. a11) (b11 +. b12)
  p7 = strassenMixed (a12 -. a22) (b21 +. b22)

-- | Mixed Strassen's matrix multiplication.
multStrassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
{-# INLINE multStrassenMixed #-}
multStrassenMixed a1@(M n m _ _ _ _) a2@(M n' m' _ _ _ _)
   | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and "
                    ++ sizeStr n' m' ++ " matrices."
   | n < strmixFactor = multStd__ a1 a2
   | otherwise =
       let mx = maximum [n,m,n',m']
           n2 = if even mx then mx else mx+1
           b1 = setSize 0 n2 n2 a1
           b2 = setSize 0 n2 n2 a2
       in  submatrix 1 n 1 m' $ strassenMixed b1 b2

-------------------------------------------------------
-------------------------------------------------------
---- NUMERICAL INSTANCE

instance Num a => Num (Matrix a) where
 fromInteger = M 1 1 0 0 1 . V.singleton . fromInteger
 negate = fmap negate
 abs = fmap abs
 signum = fmap signum

 -- Addition of matrices.
 {-# SPECIALIZE (+) :: Matrix Double -> Matrix Double -> Matrix Double #-}
 {-# SPECIALIZE (+) :: Matrix Int -> Matrix Int -> Matrix Int #-}
 {-# SPECIALIZE (+) :: Matrix Rational -> Matrix Rational -> Matrix Rational #-}
 (+) = elementwise (+)

 -- Substraction of matrices.
 {-# SPECIALIZE (-) :: Matrix Double -> Matrix Double -> Matrix Double #-}
 {-# SPECIALIZE (-) :: Matrix Int -> Matrix Int -> Matrix Int #-}
 {-# SPECIALIZE (-) :: Matrix Rational -> Matrix Rational -> Matrix Rational #-}
 (-) = elementwise (-)

 -- Multiplication of matrices.
 {-# INLINE (*) #-}
 (*) = multStrassenMixed

-------------------------------------------------------
-------------------------------------------------------
---- TRANSFORMATIONS

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix = fmap . (*)

-- | Scale a row by a given factor.
--   Example:
--
-- >              ( 1 2 3 )   (  1  2  3 )
-- >              ( 4 5 6 )   (  8 10 12 )
-- > scaleRow 2 2 ( 7 8 9 ) = (  7  8  9 )
scaleRow :: Num a => a -> Int -> Matrix a -> Matrix a
scaleRow = mapRow . const . (*)

-- | Add to one row a scalar multiple of another row.
--   Example:
--
-- >                   ( 1 2 3 )   (  1  2  3 )
-- >                   ( 4 5 6 )   (  6  9 12 )
-- > combineRows 2 2 1 ( 7 8 9 ) = (  7  8  9 )
combineRows :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineRows r1 l r2 m = mapRow (\j x -> x + l * getElem r2 j m) r1 m

-- | Switch two rows of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 4 5 6 )
-- >                ( 4 5 6 )   ( 1 2 3 )
-- > switchRows 1 2 ( 7 8 9 ) = ( 7 8 9 )
switchRows :: Int -- ^ Row 1.
           -> Int -- ^ Row 2.
           -> Matrix a -- ^ Original matrix.
           -> Matrix a -- ^ Matrix with rows 1 and 2 switched.
switchRows r1 r2 (M n m ro co w vs) = M n m ro co w $ V.modify (\mv -> do
  numLoop 1 m $ \j ->
    MV.swap mv (encode w (r1+ro,j+co)) (encode w (r2+ro,j+co))) vs

-- | Switch two coumns of a matrix.
--   Example:
--
-- >                ( 1 2 3 )   ( 2 1 3 )
-- >                ( 4 5 6 )   ( 5 4 6 )
-- > switchCols 1 2 ( 7 8 9 ) = ( 8 7 9 )
switchCols :: Int -- ^ Col 1.
           -> Int -- ^ Col 2.
           -> Matrix a -- ^ Original matrix.
           -> Matrix a -- ^ Matrix with cols 1 and 2 switched.
switchCols c1 c2 (M n m ro co w vs) = M n m ro co w $ V.modify (\mv -> do
  numLoop 1 n $ \j ->
    MV.swap mv (encode m (j+ro,c1+co)) (encode m (j+ro,c2+co))) vs

-------------------------------------------------------
-------------------------------------------------------
---- DECOMPOSITIONS

-- LU DECOMPOSITION

-- | Matrix LU decomposition with /partial pivoting/.
--   The result for a matrix /M/ is given in the format /(U,L,P,d)/ where:
--
--   * /U/ is an upper triangular matrix.
--
--   * /L/ is an /unit/ lower triangular matrix.
--
--   * /P/ is a permutation matrix.
--
--   * /d/ is the determinant of /P/.
--
--   * /PM = LU/.
--
--   These properties are only guaranteed when the input matrix is invertible.
--   An additional property matches thanks to the strategy followed for pivoting:
--
--   * /L_(i,j)/ <= 1, for all /i,j/.
--
--   This follows from the maximal property of the selected pivots, which also
--   leads to a better numerical stability of the algorithm.
--
--   Example:
--
-- >          ( 1 2 0 )     ( 2 0  2 )   (   1 0 0 )   ( 0 0 1 )
-- >          ( 0 2 1 )     ( 0 2 -1 )   ( 1/2 1 0 )   ( 1 0 0 )
-- > luDecomp ( 2 0 2 ) = ( ( 0 0  2 ) , (   0 1 1 ) , ( 0 1 0 ) , 1 )
--
--   'Nothing' is returned if no LU decomposition exists.
luDecomp :: (Ord a, Fractional a) => Matrix a -> Maybe (Matrix a,Matrix a,Matrix a,a)
luDecomp a = recLUDecomp a i i 1 1 n
 where
  i = identity $ nrows a
  n = min (nrows a) (ncols a)

recLUDecomp ::  (Ord a, Fractional a)
            =>  Matrix a -- ^ U
            ->  Matrix a -- ^ L
            ->  Matrix a -- ^ P
            ->  a        -- ^ d
            ->  Int      -- ^ Current row
            ->  Int      -- ^ Total rows
            -> Maybe (Matrix a,Matrix a,Matrix a,a)
recLUDecomp u l p d k n =
    if k > n then Just (u,l,p,d)
    else if ukk == 0 then Nothing
                     else recLUDecomp u'' l'' p' d' (k+1) n
 where
  -- Pivot strategy: maximum value in absolute value below the current row.
  i  = maximumBy (\x y -> compare (abs $ u ! (x,k)) (abs $ u ! (y,k))) [ k .. n ]
  -- Switching to place pivot in current row.
  u' = switchRows k i u
  l' = let lw = vcols l
           en = encode lw
           lro = rowOffset l
           lco = colOffset l
       in  if i == k
              then l
              else M (nrows l) (ncols l) lro lco lw $
                     V.modify (\mv -> forM_ [1 .. k-1] $ 
                                 \j -> MV.swap mv (en (i+lro,j+lco))
                                                  (en (k+lro,j+lco))
                                ) $ mvect l
  p' = switchRows k i p
  -- Permutation determinant
  d' = if i == k then d else negate d
  -- Cancel elements below the pivot.
  (u'',l'') = go u' l' (k+1)
  ukk = u' ! (k,k)
  go u_ l_ j =
    if j > nrows u_
    then (u_,l_)
    else let x = (u_ ! (j,k)) / ukk
         in  go (combineRows j (-x) k u_) (setElem x (j,k) l_) (j+1)

-- | Unsafe version of 'luDecomp'. It fails when the input matrix is singular.
luDecompUnsafe :: (Ord a, Fractional a) => Matrix a -> (Matrix a, Matrix a, Matrix a, a)
luDecompUnsafe m = case luDecomp m of
  Just x -> x
  _ -> error "luDecompUnsafe of singular matrix."

-- | Matrix LU decomposition with /complete pivoting/.
--   The result for a matrix /M/ is given in the format /(U,L,P,Q,d,e)/ where:
--
--   * /U/ is an upper triangular matrix.
--
--   * /L/ is an /unit/ lower triangular matrix.
--
--   * /P,Q/ are permutation matrices.
--
--   * /d,e/ are the determinants of /P/ and /Q/ respectively.
--
--   * /PMQ = LU/.
--
--   These properties are only guaranteed when the input matrix is invertible.
--   An additional property matches thanks to the strategy followed for pivoting:
--
--   * /L_(i,j)/ <= 1, for all /i,j/.
--
--   This follows from the maximal property of the selected pivots, which also
--   leads to a better numerical stability of the algorithm.
--
--   Example:
--
-- >           ( 1 0 )     ( 2 1 )   (   1    0 0 )   ( 0 0 1 )
-- >           ( 0 2 )     ( 0 2 )   (   0    1 0 )   ( 0 1 0 )   ( 1 0 )
-- > luDecomp' ( 2 1 ) = ( ( 0 0 ) , ( 1/2 -1/4 1 ) , ( 1 0 0 ) , ( 0 1 ) , -1 , 1 )
--
--   'Nothing' is returned if no LU decomposition exists.
luDecomp' :: (Ord a, Fractional a) => Matrix a -> Maybe (Matrix a,Matrix a,Matrix a,Matrix a,a,a)
luDecomp' a = recLUDecomp' a i i (identity $ ncols a) 1 1 1 n
 where
  i = identity $ nrows a
  n = min (nrows a) (ncols a)

-- | Unsafe version of 'luDecomp''. It fails when the input matrix is singular.
luDecompUnsafe' :: (Ord a, Fractional a) => Matrix a -> (Matrix a, Matrix a, Matrix a, Matrix a, a, a)
luDecompUnsafe' m = case luDecomp' m of
  Just x -> x
  _ -> error "luDecompUnsafe' of singular matrix."

recLUDecomp' ::  (Ord a, Fractional a)
            =>  Matrix a -- ^ U
            ->  Matrix a -- ^ L
            ->  Matrix a -- ^ P
            ->  Matrix a -- ^ Q
            ->  a        -- ^ d
            ->  a        -- ^ e
            ->  Int      -- ^ Current row
            ->  Int      -- ^ Total rows
            ->  Maybe (Matrix a,Matrix a,Matrix a,Matrix a,a,a)
recLUDecomp' u l p q d e k n =
    if k > n || u'' ! (k, k) == 0
    then Just (u,l,p,q,d,e)
    else if ukk == 0
            then Nothing
            else recLUDecomp' u'' l'' p' q' d' e' (k+1) n
 where
  -- Pivot strategy: maximum value in absolute value below the current row & col.
  (i, j) = maximumBy (comparing (\(i0, j0) -> abs $ u ! (i0,j0)))
           [ (i0, j0) | i0 <- [k .. nrows u], j0 <- [k .. ncols u] ]
  -- Switching to place pivot in current row.
  u' = switchCols k j $ switchRows k i u
  l'0 = switchRows k i l
  l' = switchCols k i l'0
  p' = switchRows k i p
  q' = switchCols k j q
  -- Permutation determinant
  d' = if i == k then d else negate d
  e' = if j == k then e else negate e
  -- Cancel elements below the pivot.
  (u'',l'') = go u' l' (k+1)
  ukk = u' ! (k,k)
  go u_ l_ h =
    if h > nrows u_
    then (u_,l_)
    else let x = (u_ ! (h,k)) / ukk
         in  go (combineRows h (-x) k u_) (setElem x (h,k) l_) (h+1)

-- CHOLESKY DECOMPOSITION

-- | Simple Cholesky decomposition of a symmetric, positive definite matrix.
--   The result for a matrix /M/ is a lower triangular matrix /L/ such that:
--
--   * /M = LL^T/.
--
--   Example:
--
-- >            (  2 -1  0 )   (  1.41  0     0    )
-- >            ( -1  2 -1 )   ( -0.70  1.22  0    )
-- > cholDecomp (  0 -1  2 ) = (  0.00 -0.81  1.15 )
cholDecomp :: (Floating a) => Matrix a -> Matrix a
cholDecomp a
        | (nrows a == 1) && (ncols a == 1) = fmap sqrt a
        | otherwise = joinBlocks (l11, l12, l21, l22) where
    (a11, a12, a21, a22) = splitBlocks 1 1 a
    l11' = sqrt (a11 ! (1,1))
    l11 = fromList 1 1 [l11']
    l12 = zero (nrows a12) (ncols a12)
    l21 = scaleMatrix (1/l11') a21
    a22' = a22 - multStd l21 (transpose l21)
    l22 = cholDecomp a22'

-------------------------------------------------------
-------------------------------------------------------
---- PROPERTIES

{-# RULES
"matrix/traceOfSum"
    forall a b. trace (a + b) = trace a + trace b

"matrix/traceOfScale"
    forall k a. trace (scaleMatrix k a) = k * trace a
  #-}

-- | Sum of the elements in the diagonal. See also 'getDiag'.
--   Example:
--
-- >       ( 1 2 3 )
-- >       ( 4 5 6 )
-- > trace ( 7 8 9 ) = 15
trace :: Num a => Matrix a -> a
trace = V.sum . getDiag

-- | Product of the elements in the diagonal. See also 'getDiag'.
--   Example:
--
-- >          ( 1 2 3 )
-- >          ( 4 5 6 )
-- > diagProd ( 7 8 9 ) = 45
diagProd :: Num a => Matrix a -> a
diagProd = V.product . getDiag

-- DETERMINANT

{-# RULES
"matrix/detLaplaceProduct"
    forall a b. detLaplace (a*b) = detLaplace a * detLaplace b

"matrix/detLUProduct"
    forall a b. detLU (a*b) = detLU a * detLU b
  #-}

-- | Matrix determinant using Laplace expansion.
--   If the elements of the 'Matrix' are instance of 'Ord' and 'Fractional'
--   consider to use 'detLU' in order to obtain better performance.
--   Function 'detLaplace' is /extremely/ slow.
detLaplace :: Num a => Matrix a -> a
detLaplace m@(M 1 1 _ _ _ _) = m ! (1,1)
detLaplace m = sum1 [ (-1)^(i-1) * m ! (i,1) * detLaplace (minorMatrix i 1 m) | i <- [1 .. nrows m] ]
  where
    sum1 = foldl1' (+)

-- | Matrix determinant using LU decomposition.
--   It works even when the input matrix is singular.
detLU :: (Ord a, Fractional a) => Matrix a -> a
detLU m = case luDecomp m of
  Just (u,_,_,d) -> d * diagProd u
  Nothing -> 0
