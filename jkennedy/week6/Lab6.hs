module Lab6

where
import Data.List
import System.Random
import Week6
import System.CPUTime
import Text.Printf
import Control.Monad

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

--exercise 4, 1 hour
--because my exM function sucks I took one from the internet to help me increase the performance
testFermatComposite :: IO Integer
testFermatComposite = testFermat composites

testFermat :: [Integer] -> IO Integer
testFermat (x:xs) = do
					a <- primeF 50 x
					if a  == True then return x
					else testFermat xs
								
--k = 1 gives 4, k = 2 gives 4, k = 3 gives	4
--increasing k increases the run time, but also gives a higher chance of a returned value to be an
--actual prime.		

--exercise 5, for some reason some carmichael numbers return false with the primeF test. I think something 
--broke :o 

--exercise 6, can't find anything ...
testMRCar ::IO Integer
testMRCar = testMR carmichael

testMR :: [Integer] -> IO Integer
testMR (x:xs) = do
					a <- primeMR 5 x
					if a  == True then return x
					else testMR xs			
								
--exercise 7, 30 min								
findMersPrime :: Int -> IO [Integer]
findMersPrime n = do
				filterM (primeMR 5) $ take n mersenne
				where mersenne = map (\p -> 2^p - 1) primes
				
--All the results found were mersenne primes, I didn't go on the long because my pc couldn't handle it after a while

				