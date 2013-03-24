
import Criterion.Main
--
import Data.Matrix

mat :: Int -> Matrix Int
mat n = matrix n n $ \(i,j) -> i - j

testdef :: Int -> Matrix Int
testdef n = multStd (mat n) (mat n)

teststr :: Int -> Matrix Int
teststr n = multStrassen (mat n) (mat n)

teststrm :: Int -> Matrix Int
teststrm n = multStrassenMixed (mat n) (mat n)

bmat :: Int -> Benchmark
bmat n = bgroup ("mult" ++ show n)
 [ bench "Definition" $ nf testdef n
 , bench "Strassen mixed" $ nf teststrm n
 ]

main :: IO ()
main = defaultMain $ fmap bmat [10,25,100,250,500]