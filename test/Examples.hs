
import Data.Matrix
import System.Exit (exitFailure)
import System.IO (hFlush,stdout)

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
      ( mapRow (\_ x -> x + 1) 2 $ fromList 3 3 [1..9]
      , fromList 3 3 [1,3,3 , 4,6,6 , 7,9,9]
        )
  , testEquality "submatrix"
      ( submatrix 1 2 2 3 $ fromList 3 3 [1..9]
      , fromList 2 2 [2,3 , 5,6]
        )
  , testEquality "minorMatrix"
      ( minorMatrix 2 2 $ fromList 3 3 [1..9]
      , fromList 2 2 [1,3 , 7,9]
        )
    ]
