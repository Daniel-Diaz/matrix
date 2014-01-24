
import Data.Matrix
import Data.Ratio
import Control.Applicative

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

{- matrix package test set

This program uses QuickCheck to check that the matrix
functions of the matrix package are working properly.

We use the type Rational to have avoid numerical errors
that may cause the test to fail while the algorithm is
correct.

-}

-- | Numbers used in tests.
type R = Rational

newtype I = I { fromI :: Int }

instance Show I where
  show (I n) = show n

instance Arbitrary I where
  arbitrary = I <$> choose (1,9)

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    I n <- arbitrary
    I m <- arbitrary
    genMatrix' n m

genMatrix' :: Arbitrary a => Int -> Int -> Gen (Matrix a)
genMatrix' n m = fromList n m <$> vector (n*m)

genMatrix :: Int -> Int -> Gen (Matrix R)
genMatrix = genMatrix'


-- | Square matrices
newtype Sq = Sq { fromSq :: Matrix R }

instance Show Sq where
  show (Sq m) = show m

instance Arbitrary Sq where
  arbitrary = do
    I n <- arbitrary
    Sq <$> genMatrix n n

main :: IO ()
main = defaultMain $ testGroup "matrix tests" [
    QC.testProperty "identity * m = m * identity = m"
       $ \(Sq m) -> let n = nrows m in identity n * m == m && m * identity n == m
  , QC.testProperty "permMatrix n i j * permMatrix n i j = identity n"
       $ \(I n) -> forAll (choose (1,n))
       $ \i     -> forAll (choose (1,n))
       $ \j     -> permMatrix n i j * permMatrix n i j == identity n
  , QC.testProperty "setElem (getElem i j m) (i,j) m = m"
       $ \m -> forAll (choose (1,nrows m))
       $ \i -> forAll (choose (1,ncols m))
       $ \j -> setElem (getElem i j m) (i,j) m == (m :: Matrix R)
  , QC.testProperty "transpose (transpose m) = m"
       $ \m -> transpose (transpose m) == (m :: Matrix R)
  , QC.testProperty "joinBlocks (splitBlocks i j m) = m"
       $ \m -> forAll (choose (1,nrows m))
       $ \i -> forAll (choose (1,ncols m))
       $ \j -> joinBlocks (splitBlocks i j m) == (m :: Matrix R)
  , QC.testProperty "(+) = elementwise (+)"
       $ \m1 -> forAll (genMatrix (nrows m1) (ncols m1))
       $ \m2 -> m1 + m2 == elementwise (+) m1 m2
  , QC.testProperty "if (u,l,p,d) = luDecomp m then (p*m = l*u) && (detLaplace p = d)"
       $ \(Sq m) -> (detLaplace m /= 0) ==>
             (let (u,l,p,d) = luDecompUnsafe m in p*m == l*u && detLaplace p == d)
  , QC.testProperty "detLaplace m = detLU m"
       $ \(Sq m) -> detLaplace m == detLU m
  , QC.testProperty "if (u,l,p,q,d,e) = luDecomp' m then (p*m*q = l*u) && (detLU p = d) && (detLU q = e)"
       $ \(Sq m) -> (detLU m /= 0) ==>
             (let (u,l,p,q,d,e) = luDecompUnsafe' m in p*m*q == l*u && detLU p == d && detLU q == e)
  , QC.testProperty "detLU (scaleRow k i m) = k * detLU m"
       $ \(Sq m) k -> forAll (choose (1,nrows m))
       $ \i -> detLU (scaleRow k i m) == k * detLU m
  , QC.testProperty "let n = nrows m in detLU (switchRows i j m) = detLU (permMatrix n i j) * detLU m"
       $ \(Sq m) -> let n = nrows m in forAll (choose (1,n))
       $ \i      -> forAll (choose (1,n))
       $ \j      -> detLU (switchRows i j m) == detLU (permMatrix n i j) * detLU m
  , QC.testProperty "switchCols i j = transpose . switchRows i j . transpose"
       $ \m -> forAll (choose (1,ncols m))
       $ \i -> forAll (choose (1,ncols m))
       $ \j -> switchCols i j (m :: Matrix R) == (transpose $ switchRows i j $ transpose m)
    ]
