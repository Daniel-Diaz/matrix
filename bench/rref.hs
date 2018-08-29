
import Criterion.Main
--
import Data.Matrix

mat :: Int -> Matrix Double
mat n = fromList n n $ map (^2) [1..]

testrref :: Int -> Either String (Matrix Double)
testrref n = rref (mat n)

testrref' :: Int -> Either String (Matrix Double)
testrref' n = rref' (mat n)

brref :: Int -> Benchmark
brref n = bgroup ("rref" ++ show n)
 [ bench "rref" $ nf testrref n
 , bench "rosetta rref'" $ nf testrref' n
 ]

main :: IO ()
main = defaultMain $ fmap brref [10,25,100]
