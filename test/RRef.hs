
import Data.Matrix
import Data.Either (fromRight)
import Data.Ratio
import System.Exit (exitFailure)
import System.IO (hFlush,stdout)

type R = Ratio Integer

-- We flush stdout explictly to get output printed
-- in real-time in Windows systems.
putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

testEquality :: (Show a, Eq a) => String -> (a,a) -> IO ()
testEquality n (x,y) = if x == y
    then putStrLn "OK."
    else (putStrLn $ "Failed, got " ++ show x ++ " instead of " ++ show y) >> exitFailure

main :: IO ()
main = sequence_
  [ testEquality "rref bug #42" $
    ( rref' (fromList 9 10
                [(-1),1,0,0,0,0,0,0,1,1
                ,1,(-1),0,0,0,0,1,0,1,1
                ,1,1,(-1),0,0,0,0,1,0,1
                ,0,0,1,(-1),0,0,0,0,0,1
                ,0,0,0,1,(-1),1,0,0,0,1
                ,0,0,0,0,0,(-1),1,0,0,1
                ,0,0,0,0,0,0,(-1),0,1,1
                ,0,0,0,0,0,0,0,(-1),0,1
                ,0,0,0,0,0,0,0,0,(-1),1
                ] :: Matrix R)
    , Right (fromList 9 10
        [1,0,0,0,(-1)%2,0,0,0,0,0
        ,0,1,0,0,(-1)%2,0,0,0,0,0
        ,0,0,1,0,(-1),0,0,0,0,0
        ,0,0,0,1,(-1),0,0,0,0,0
        ,0,0,0,0,0,1,0,0,0,0
        ,0,0,0,0,0,0,1,0,0,0
        ,0,0,0,0,0,0,0,1,0,0
        ,0,0,0,0,0,0,0,0,1,0
        ,0,0,0,0,0,0,0,0,0,1
        ] :: Matrix R)
    )
  , testEquality "Exception from Reduced Row Echelon Form Conversion #52" $
    ( rref' $ fromList 2 3 [1,2,3,2,4,6]
    , Right $ fromList 2 3 [1,2,3,0,0,0]
    )
  ]
