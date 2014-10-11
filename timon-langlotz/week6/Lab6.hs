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
        x <- getRandomInt 10000000000
        y <- getRandomInt 50000000
        m <- getRandomInt 100000
        putStrLn "Testing for:"
        putStr . show $ x
        putStr "^"
        putStr . show $ y
        putStr " mod "
        print m
        
        putStrLn "Test exM: "
        start1 <- getCurrentTime
        putStrLn $ show $ exM x y m
        end1 <- getCurrentTime
        putStrLn $ show $ diffUTCTime end1 start1
        
        putStrLn "Test expM"
        start2 <- getCurrentTime
        putStrLn $ show $ expM x y m
        end2 <- getCurrentTime
        putStrLn $ show $ diffUTCTime end2 start2

        
test_exM (Positive x) (Positive y) (Positive m) = (expM x y m) == (exM x y m)

getRandomInt :: Integer -> IO Integer
getRandomInt n = getStdRandom (randomR (0,n))

-- Exercise 3
composites :: [Integer]
composites = sieve2 [2..] primes

sieve2 :: [Integer] -> [Integer] -> [Integer]
sieve2 list@(n:ns) (p:ps) = (takeWhile (<p) list) ++ 
                                if n == p
                                then sieve2 ns ps
                                else sieve2 (dropWhile (<=p) list) ps

-- Exercise 4 + 5
-- primeF -> steps -> number
testFalsePrime :: Int -> [Integer] -> IO ()
testFalsePrime steps (c:cs) = do
                b <- primeF steps c
                if b then 
                    putStrLn $ show $ c
                else
                    testFalsePrime steps cs
 

-- Exercise 5









