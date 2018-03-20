
import Criterion.Main
--
import Data.Matrix
import qualified Data.Matrix.Unboxed as U

mat :: Int -> Matrix Int
mat n = fromList n n [1..]

testdef :: Int -> Matrix Int
testdef n = multStd (mat n) (mat n)

testdef2 :: Int -> Matrix Int
testdef2 n = multStd2 (mat n) (mat n)

teststr :: Int -> Matrix Int
teststr n = multStrassen (mat n) (mat n)

teststrm :: Int -> Matrix Int
teststrm n = multStrassenMixed (mat n) (mat n)


matU :: Int -> U.Matrix Int
matU n = U.fromList n n [1..]

testdefU :: Int -> U.Matrix Int
testdefU n = U.multStd (matU n) (matU n)

testdef2U :: Int -> U.Matrix Int
testdef2U n = U.multStd2 (matU n) (matU n)

teststrU :: Int -> U.Matrix Int
teststrU n = U.multStrassen (matU n) (matU n)

teststrmU :: Int -> U.Matrix Int
teststrmU n = U.multStrassenMixed (matU n) (matU n)


bmat :: Int -> Benchmark
bmat n = bgroup ("mult" ++ show n)
 [ bench "Definition" $ nf testdef n
 , bench "Definition U" $ nf testdefU n
 , bench "Definition 2" $ nf testdef2 n
 , bench "Definition 2 U" $ nf testdef2U n
{-
Strassen and StrassenU tests are commented out because they are consistentlty slower,
by several orders of magnitude, than other tests, and use /much/ more memory,
to the point that they prevent the benchmark from finishing when the matrix size is > 300.
-}
-- , bench "Strassen" $ nf teststr n
-- , bench "StrassenU" $ nf teststrU n
 , bench "Strassen mixed" $ nf teststrm n
 , bench "Strassen mixed U" $ nf teststrmU n
 ]

main :: IO ()
main = defaultMain $ fmap bmat [10,25,100,150,250,400,500]
