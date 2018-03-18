
import Data.Matrix
import System.Exit (exitFailure)
import System.IO (hFlush,stdout)
import Data.Either (isLeft)

-- We flush stdout explictly to get output printed
-- in real-time in Windows systems.
putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

testExample :: String -> Bool -> IO ()
testExample n b = do
  putStr' $ "Test (" ++ n ++ "): "
  if b then putStrLn "OK."
       else putStrLn "Failed." >> exitFailure

testEquality :: Eq a => String -> (a,a) -> IO ()
testEquality n (x,y) = testExample n $ x == y

main :: IO ()
main = sequence_
  [ testEquality "matrix"
      ( matrix 4 4 $ \(i,j) -> 2*i - j
      , fromList 4 4
          [ 1 , 0 , -1 , -2
          , 3 , 2 ,  1 ,  0
          , 5 , 4 ,  3 ,  2
          , 7 , 6 ,  5 ,  4 ]
        )
  , testEquality "fromList"
      ( fromList 3 3 [1..]
      , fromList 3 3
          [ 1 , 2 , 3
          , 4 , 5 , 6
          , 7 , 8 , 9 ]
        )
  , testEquality "fromLists (1)"
      ( fromLists [ [1,2,3] , [4,5,6] , [7,8,9] ]
      , fromList 3 3
          [ 1 , 2 , 3
          , 4 , 5 , 6
          , 7 , 8 , 9 ]
        )
  , testEquality "fromLists (2)"
      ( fromLists [ [1,2,3] , [4,5,6,7] , [8,9,0] ]
      , fromList 3 3
          [ 1 , 2 , 3
          , 4 , 5 , 6
          , 8 , 9 , 0 ]
        )
  , testEquality "identity"
      ( identity 3 , fromList 3 3 [1,0,0 , 0,1,0 , 0,0,1]
        )
  , testEquality "diagonalList"
      ( diagonalList 3 0 [1..] , fromList 3 3 [1,0,0 , 0,2,0 , 0,0,3]
        )
  , testEquality "transpose"
      ( transpose $ fromList 3 3 [1..9]
      , fromList 3 3 [1,4,7 , 2,5,8 , 3,6,9]
        )
  , testEquality "extendTo"
      ( extendTo 0 4 5 $ fromList 3 3 [1..9]
      , fromList 4 5
          [ 1 , 2 , 3 , 0 , 0
          , 4 , 5 , 6 , 0 , 0
          , 7 , 8 , 9 , 0 , 0
          , 0 , 0 , 0 , 0 , 0
            ]
        )
  , testEquality "mapRow"
      ( mapRow (\_ x -> x + 1) 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [1,2,3 , 5,6,7 , 7,8,9]
        )
  , testEquality "mapCol"
      ( mapCol (\_ x -> x + 1) 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [1,3,3 , 4,6,6 , 7,9,9]
        )
  , testEquality "mapPos"
      ( mapPos (\(r,c) x -> r + 2 * c) $ fromList 2 2 [1..4]
      , fromList 2 2 [3, 5, 4, 6]
      )
  , testEquality "submatrix"
      ( submatrix 1 2 2 3 $ fromList 3 3 [1..9]
      , fromList 2 2 [2,3 , 5,6]
        )
  , testEquality "minorMatrix"
      ( minorMatrix 2 2 $ fromList 3 3 [1..9]
      , fromList 2 2 [1,3 , 7,9]
        )
  , testEquality "scaleMatrix"
      ( scaleMatrix 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [2,4,6 , 8,10,12 , 14,16,18]
        )
  , testEquality "scaleRow"
      ( scaleRow 2 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [1,2,3 , 8,10,12 , 7,8,9]
        )
  , testEquality "combineRows"
      ( combineRows 2 2 1 $ fromList 3 3 [1..9]
      , fromList 3 3 [1,2,3 , 6,9,12 , 7,8,9]
        )
  , testEquality "switchRows"
      ( switchRows 1 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [4,5,6 , 1,2,3 , 7,8,9]
        )
  , testEquality "switchCols"
      ( switchCols 1 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [2,1,3 , 5,4,6 , 8,7,9]
        )
  , testEquality "toList"
      ( toList $ fromList 3 3 [1..9]
      , [1..9]
        )
  , testEquality "toLists"
      ( toLists $ fromList 3 3 [1..9]
      , [ [1,2,3] , [4,5,6] , [7,8,9] ]
        )
  , testEquality "inverse (1)"
      ( inverse $ fromList 2 2 [1,7, 2,4]
      , Right $ fromList 2 2 [-4/10,7/10, 2/10,-1/10] :: Either String (Matrix Rational)
        )
  , testEquality "inverse (2)"
      ( inverse $ fromList 3 3 [1,7,-12,  2,4,10,  0,-23,1]
      , Right $ fromList 3 3 [117/386, 269/772, 59/386,
            -1/386, 1/772, -17/386,
            -23/386, 23/772, -5/386] :: Either String (Matrix Rational))
  , testEquality "inverse (3)"
      ( inverse $ fromList 4 4 [1,2345,23,78,   12,34556,123,-1242,   429,-131,0,0,  0,0,0,-1]
      , Right $ fromList 4 4 [
            -5371/72415160, 3013/217245480, 506353/217245480, -41658/1810379,
            -17589/72415160, 3289/72415160, -51/72415160, -136422/1810379,
            617754/9051895, -125767/27155685, -802/27155685, 20050470/1810379,
            0, 0, 0, -1] :: Either String (Matrix Rational))
  , testEquality "inverse (4)"
      ( inverse $ fromList 2 2 [0,1, 1,0]
      , Right $ fromList 2 2 [0,1, 1,0] :: Either String (Matrix Rational))
  , testEquality "inverse (5)"
      ( inverse $ fromList 3 3 [1,0,0, 0,0,1, 0,1,0]
      , Right $ fromList 3 3 [1,0,0, 0,0,1, 0,1,0] :: Either String (Matrix Rational))
  , testExample "inverse (6)" $ isLeft $ inverse $ fromList 2 2 [1,1,2,2]
  , testEquality "inverse (7)" ( inverse $ fromList 3 3 [0,0,0, 0,0,0, 0,0,0 :: Double], Left "Attempt to invert a non-invertible matrix")
    ]
