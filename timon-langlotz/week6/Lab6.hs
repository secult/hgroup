module Lab6

where
import Data.List
import Data.Time
import System.Random
import Test.QuickCheck
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Exercise 1
-- exM in Week6.hs
           
-- Exercise 2
measureSpeed :: IO ()
measureSpeed = do 
        x <- getRandomInt 100000
        y <- getRandomInt 50000
        m <- getRandomInt 10000
        putStrLn "Test exM"
        start1 <- getCurrentTime
        putStrLn $ show $ exM x y m
        end1 <- getCurrentTime
        putStrLn $ show $ diffUTCTime end1 start1
        
        putStrLn "Test expM"
        start2 <- getCurrentTime
        putStrLn $ show $ expM x y m
        end2 <- getCurrentTime
        putStrLn $ show $ diffUTCTime end2 start2

testInvariant x y m = (expM x y m) == (exM x y m)

getRandomInt :: Integer -> IO Integer
getRandomInt n = getStdRandom (randomR (0,n))