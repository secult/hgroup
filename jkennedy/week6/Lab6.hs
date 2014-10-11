module Lab6

where
import Data.List
import System.Random
import Week6
import System.CPUTime
import Text.Printf

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

--exercise 1, 1 hour 
--implemented in Week6
{-exM :: Integer -> Integer -> Integer -> Integer
exM x exp modulo = rem (product (map (\y -> rem y modulo) powers)) modulo
				where powers = powersOfInt x exp

largestPower :: Integer -> [Integer]
largestPower 0 = []
largestPower exp = largest : largestPower (exp - largest)
				where largest = last [2^x | x <- [0..exp], 2^x <= exp]
				
powersOfInt :: Integer -> Integer -> [Integer]
powersOfInt x exp = [x^y | y <- [2^z | z <- [0..exp]], elem y (largestPower exp)]
-}

--exercise 2, 10 min. Not efficient at all... me no understand
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

exercise2 = do
	putStrLn "exM 1245 35 2537"
	time $ exM 298834816205857413691589142149881946 235139930337346448646612253341554407 (10 ^ 40) `seq` return()
	putStrLn "Done"
	
	putStrLn "expM 1245 35 2537"
	time $ expM 298834816205857413691589142149881946 235139930337346448646612253341554407 (10 ^ 40) `seq` return()
	putStrLn "Done"

--exercise 3, 30 min (because I was thinking about using sieve... decided to fuck it xD)
composites :: [Integer]
composites = [x | x <- [2..], not $ isPrime x]

--exercise 4