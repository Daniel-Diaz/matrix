{-# LANGUAGE FlexibleInstances #-}
import Data.Matrix
import Data.Ratio
import Control.Applicative
import Data.Monoid

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec

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

instance CoArbitrary a => CoArbitrary (Matrix a) where
  coarbitrary = coarbitrary . toList
instance Show (Int -> Int) where
  show _ = ""

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    I n <- arbitrary
    I m <- arbitrary
    genMatrix' n m

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

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
main = hspec $ parallel $ describe "matrix tests" $do 
    it "zero n m = matrix n m (const 0)"$ property
       $ \(I n) (I m) -> zero n m == matrix n m (const 0)
    it "identity * m = m * identity = m" $ property
       $ \(Sq m) -> let n = nrows m in identity n * m == m && m * identity n == m
    it "a * (b * c) = (a * b) * c" $ property
       $ \(I a) (I b) (I c) (I d) -> forAll (genMatrix a b)
       $ \m1 -> forAll (genMatrix b c)
       $ \m2 -> forAll (genMatrix c d)
       $ \m3 -> m1 * (m2 * m3) == (m1 * m2) * m3
    it "multStd a b = multStd2 a b" $ property
       $ \(I a) (I b) (I c) -> forAll (genMatrix a b)
       $ \m1 -> forAll (genMatrix b c)
       $ \m2 -> multStd m1 m2 == multStd2 m1 m2
    it "getMatrixAsVector m = mconcat [ getRow i m | i <- [1 .. nrows m]]" $ property
       $ \m -> getMatrixAsVector (m :: Matrix R) == mconcat [ getRow i m | i <- [1 .. nrows m] ]
    it  "fmap id = id" $ property
       $ \m -> fmap id m == (m :: Matrix R)
    it "permMatrix n i j * permMatrix n i j = identity n" $ property 
       $ \(I n) -> forAll (choose (1,n))
       $ \i     -> forAll (choose (1,n))
       $ \j     -> permMatrix n i j * permMatrix n i j == identity n
    it  "setElem (getElem i j m) (i,j) m = m" $ property
       $ \m -> forAll (choose (1,nrows m))
       $ \i -> forAll (choose (1,ncols m))
       $ \j -> setElem (getElem i j m) (i,j) m == (m :: Matrix R)
    it "transpose (transpose m) = m" $ property 
       $ \m -> transpose (transpose m) == (m :: Matrix R)
    it "if m' = setSize e r c m then (nrows m' = r) && (ncols m' = c)" $ property
       $ \e (I r) (I c) m -> let m' :: Matrix R ; m' = setSize e r c m in nrows m' == r && ncols m' == c
    it "if (nrows m = r) && (nrcols m = c) then setSize _ r c m = m" $ property
       $ \m -> let r = nrows m
                   c = ncols m
               in  setSize undefined r c m == (m :: Matrix R)
    it "getRow i m = getCol i (transpose m)" $ property
       $ \m -> forAll (choose (1,nrows m))
       $ \i -> getRow i (m :: Matrix R) == getCol i (transpose m)
    it "joinBlocks (splitBlocks i j m) = m" $ property
       $ \m -> forAll (choose (1,nrows m))
       $ \i -> forAll (choose (1,ncols m))
       $ \j -> joinBlocks (splitBlocks i j m) == (m :: Matrix R)
    it "scaleMatrix k m = fmap (*k) m" $ property
       $ \k m -> scaleMatrix k m == fmap (*k) (m :: Matrix R)
    it "(+) = elementwise (+)" $ property
       $ \m1 -> forAll (genMatrix (nrows m1) (ncols m1))
       $ \m2 -> m1 + m2 == elementwise (+) m1 m2
    it"switchCols i j = transpose . switchRows i j . transpose" $ property
       $ \m -> forAll (choose (1,ncols m))
       $ \i -> forAll (choose (1,ncols m))
       $ \j -> switchCols i j (m :: Matrix R) == (transpose $ switchRows i j $ transpose m)
    it"detLaplace (fromList 3 3 $ repeat 1) = 0"$ property
       $ detLaplace (fromList 3 3 $ repeat 1) == 0
    it "if (u,l,p,d) = luDecomp m then (p*m = l*u) && (detLaplace p = d)" $ property
       $ \(Sq m) -> (detLaplace m /= 0) ==>
             (let (u,l,p,d) = luDecompUnsafe m in p*m == l*u && detLaplace p == d)
    it "detLaplace m = detLU m" $ property
       $ \(Sq m) -> detLaplace m == detLU m
    it "if (u,l,p,q,d,e) = luDecomp' m then (p*m*q = l*u) && (detLU p = d) && (detLU q = e)" $ property
       $ \(Sq m) -> (detLU m /= 0) ==>
             (let (u,l,p,q,d,e) = luDecompUnsafe' m in p*m*q == l*u && detLU p == d && detLU q == e)
    it "detLU (scaleRow k i m) = k * detLU m" $ property
       $ \(Sq m) k -> forAll (choose (1,nrows m))
       $ \i -> detLU (scaleRow k i m) == k * detLU m
    it "let n = nrows m in detLU (switchRows i j m) = detLU (permMatrix n i j) * detLU m"  $ property
       $ \(Sq m) -> let n = nrows m in forAll (choose (1,n))
       $ \i      -> forAll (choose (1,n))
       $ \j      -> detLU (switchRows i j m) == detLU (permMatrix n i j) * detLU m
    it  "fromList n m . toList = id" $ property
       $ \m -> fromList (nrows m) (ncols m) (toList m) == (m :: Matrix R)
    it "fromLists . toLists = id" $ property 
       $ \m -> fromLists (toLists m) == (m :: Matrix (Sum Int))
    it "monoid law: mappend mempty x = x" $ property
       $ \x -> mappend mempty (x :: Matrix (Sum Int)) == x
    it "monoid law: mappend x mempty = x" $ property
       $ \x -> mappend (x :: Matrix (Sum Int)) mempty == x
    it "monoid law: mappend x (mappend y z) = mappend (mappend x y) z " $ property
       $ \x y z -> mappend (x :: Matrix (Sum Int)) (mappend (y::Matrix (Sum Int)) (z::Matrix (Sum Int))) == mappend (mappend x y) z
    it "applicative law - identity: pure id <*> v = v" $ property
       $ \x -> (pure id <*> (x :: (Matrix Int))) == x
    it "applicative law - composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ property
       $ \u v w -> (pure (.) <*> (u :: Matrix (Int->Int)) <*> (v :: Matrix (Int->Int)) <*> (w :: Matrix Int)) == (u <*> (v <*> w))
    it "applicative law - homomorphism: pure f <*> pure x = pure (f x)" $ property
       $ \f x -> ((pure (f :: Int -> Int) <*> pure (x :: Int))::Matrix Int) == pure (f x)
    it "applicative law - interchange: u <*> pure y = pure ($ y) <*> u" $ property
       $ \u y -> ((u :: Matrix (Int -> Int)) <*> pure (y :: Int)) == (pure ($ y ) <*> u)
