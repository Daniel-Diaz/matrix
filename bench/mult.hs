
import Criterion.Main
--
import Data.Matrix

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

bmat :: Int -> Benchmark
bmat n = bgroup ("mult" ++ show n)
 [ -- testdef seems to be the slowest in every situation.
   -- We comment it to speed up benchmarking.
   -- bench "Definition" $ nf testdef n
   bench "Definition 2" $ nf testdef2 n
 -- , bench "Strassen" $ nf teststr n
 , bench "Strassen mixed" $ nf teststrm n
 ]

main :: IO ()
main = defaultMain $ fmap bmat [10,25,100,150,250,400,500]
