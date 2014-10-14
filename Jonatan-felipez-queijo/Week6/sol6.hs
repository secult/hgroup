module Sol6

where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Week6
import Lab6
import Data.Time
import Control.DeepSeq

-- exercise1
myexM :: Integer -> Integer -> Integer -> Integer
myexM _ 0 _ = 1
myexM x y m | y `rem` 2 == 0 = (myexM x (y `div` 2) m)^2 `rem` m -- utilize that x^4 == (x^2)^2
            | otherwise      = multM (myexM x (y-1) m) x m       -- utilize that x^3 = x*x^2

-- function can also be found in week6 module: expM          

-- I kept this function to compare it later to myexM       
slowexM ::  Integer -> Integer -> Integer -> Integer
slowexM x y = rem (x^y)            

-- simple test to convince myself that myexM is correct if slowexM is correct
myexMYieldsslowxM2 :: Integer -> Integer -> Integer -> Bool
myexMYieldsslowxM2 x y m | m <= 0 || x < 0 || y < 0 = True -- mod zero, negative x, y exception
                         | otherwise                = myexM x y m == slowexM x y m
                       
-- exercise 2
{--
   performanceTest:
   My hypothesis is that the performance of myexM will be faster (compared to slowExm), 
   if the power that has to be calculated is a big number. 
--}                 
testexMperf :: IO()
testexMperf = do 
     let xs = [((x,y),z) | x <- ([2..80]            :: [Integer]), 
                           y <- ([1000000..1000500] :: [Integer]), 
                           z <- ([2..100]           :: [Integer])]
     
     putStrLn "testing myexM function, please wait..."
     start <- getCurrentTime
     let result = (map (\((a,b),c) -> myexM a b c) xs)
     result `deepseq` (putStrLn "done! Time it took:") -- deepseq to force full evaluation without printing.
     end <- getCurrentTime
     putStrLn $ "slowExm took: " ++ (show $ diffUTCTime end start)
     
     putStrLn $ "warning, the slow function will take a long time. are your sure (y/n)?"
     x <- readLn
     
     if x == "y" then do
         putStrLn "testing slowexM function, please wait..."
         start <- getCurrentTime
         let result = (map (\((a,b),c) -> myexM a b c) xs)
         result `deepseq` (putStrLn "done! Time it took:") -- deepseq to force full evaluation without printing.
         end <- getCurrentTime
         putStrLn $ "slowExm took: " ++ (show $ diffUTCTime end start)
     else return ()
     
     

      
{--
    Testresults:
    It took the slowExm function:
    Did not terminate after 45 minutes, so stopped.
    
    myExm took:
    381 seconds (At least, that's what diffUTCTime reported)
    
    And thus my hypothesis seems to be correct
    Function
   
--}

--exercise 3
{-- 
    Although the exercise adviced to use the sieve, I though this implementation was clearer.
    The function takes care of skipping the primes while building up the list, by checking 
    if the number is divisible by any number below halve of it.
--}
composite = [ n | n <- [4..], any (\m -> rem n m == 0) [2 .. (div (n-1) 2)]] 



-- exercise 4
-- infinitely try to find and print Fermat test errors 
printFermatCoError :: IO ()
printFermatCoError = do
               x <- testFermatComposite 3
               putStrLn $ show x
               printFermatCoError

testFermatComposite :: Int -> IO Integer
testFermatComposite = getFirstContr composite

getFirstContr :: [Integer] -> Int -> IO Integer
getFirstContr (y:ys) k = do
                           x <- primeF k y
                           if x then return y
                           else getFirstContr ys k

{-- 
    For steps [1..10] 4 was the first false positive found in reasonable time. 
    This makes sense, because the algorithm is probabilistic and the possible 
    witnesses are not picked distinctly, so it is possible to repeatedly pick a liar
    
    my observation is that by increasing the trials (steps), the test gets more accurate.
    This makes sence, since at most half of the possible witnesses are liars 
    khan academy:
    https://www.khanacademy.org/computing/computer-science/cryptography/random-algorithms-probability/v/fermat-primality-test-prime-adventure-part-10
    
    (also a nice explanation of why fermats little theorem works: 
    https://www.khanacademy.org/math/recreational-math/math-warmup/number-theory-warmups/v/fermat-s-little-theorem-visualization)
--}

-- exercise 5 
{--
    Since carmichael numbers are made to fool the fermat test, 
    my hypothesis is that the fermat tests will always be fooled by a carmichael number.
--}


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

testFermatCarmichael :: Int -> IO Integer
testFermatCarmichael = getFirstContr carmichael

printFermatCaError :: IO ()
printFermatCaError = do
               x <- testFermatCarmichael 3
               putStrLn $ show x
               printFermatCaError
               
{-- 
    After doing the tests above, my hypothesis was proven wrong since sometimes carmichael 
    numbers are detected as composites. After examining the fermat test function in lab6, 
    I noticed the check was "a^(p-1) = 1 mod p" and this is the reason the carmichael numbers 
    sometimes get detected as non-primes:
        "a^p = a mod p" can be rewritten as "a^(p-1) = 1 mod p" only if a and p are coprime 
        (since this step is a modular division).
    
    since carmichael numbers are composite, we do not have the guarantee of modular division 
    and thus there exist fermat witnesses for a carmichael number. 
    (E.g.: the prime factors that build up the number are not coprime with it).
--}
               
-- Exercise 6
{-- 
    Looking at the conclusion of exercise 5, and the fact that MR is an extended fermat test, 
    I expect that it is still possible to detect carmichael numbers as composite within 
    reasonable time and that with the extended check this will occur more often than with the fermat test.
--}


{-- 
    
--}

-- Exercise 7
{-- 
    Experimented with parallel mersennePrime checking, but left it out eventually.
    Left the computer on doing this while sleeping.
--}

myexp :: Integer -> Integer -> Integer
myexp _ 0 = 1
myexp x y | y `rem` 2 == 0 = (myexp x (div y 2))^2 -- utilize that x^4 == (x^2)^2
          | otherwise      = (myexp x (y-1)    )*x -- utilize that x^3 = x*x^2 

-- primes to pick from (start looking after m24      
primeSearchList = take 8500 $ drop 2254 primes

mersennePrime :: IO ()
mersennePrime = do 
    num <- getStdRandom (randomR (0,8499))
    let p = primeSearchList !! num
    let maybeP = (myexp 2 p) - 1
    putStrLn $ "testing: 2^" ++ show p ++ "-1"
    foundPrime <- primeMR 8 maybeP
    if (foundPrime) 
    then do 
        putStrLn $ show (p,maybeP)
        mersennePrime
    else mersennePrime

-- found M10, M4, M7, M12, M13, M14, M15. Then tried finding M25 or more it found
-- M27 and M29 (Checked after 10 hours)

-- Exercise 8

--shamelessly copied the interactive stuff from Timon and afterwards refactored a bit.

rsaKeys :: Int -> IO ()
rsaKeys bits = do
            putStrLn "Generating keys..."
            p <- primeWithBits bits
            putStrLn "(1/2)"
            q <- primeWithBits bits
            putStrLn "(2/2)"
            let (e,n) = rsa_public p q
            let (d,n) = rsa_private p q
            putStrLn "Enter the message you wish to encode: "
            message <- getLine
            let msgInt = messageToInteger message
            let cipher = rsa_encode (e,n) msgInt
            putStrLn "Encoded message: "
            print cipher 
            putStrLn "Do you want to decode again? (y/n) "
            dec <- getLine
            if dec == "y" then do
                putStrLn "Decoding cipher "
                print cipher
                let orgMsg = rsa_decode (d,n) (cipher)
                print $ integerToMessage orgMsg
            else
                putStrLn "Bye!"

-- generate probable prime with at most x bits                
primeWithBits :: Int -> IO Integer
primeWithBits bits = do
     x <- getStdRandom (randomR (0,2^bits -1))
     if odd x then do
        maybePrime <- primeMR 10 x
        if maybePrime then
            return x
        else
            primeWithBits bits
     else
        primeWithBits bits

-- Make a message (string) into an Integer        
messageToInteger :: String -> Integer
messageToInteger = binToInt.concat.(map charToBin)

-- Make a charactar binary
charToBin :: Char -> [Int]
charToBin = fill.reverse.decToBin.ord 

-- Make an integer binary
decToBin :: Int -> [Int]
decToBin 0 = []
decToBin c = let (a,b) = quotRem c 2 
             in [b] ++ decToBin a

-- Add padding to bytes if necessary             
fill :: [Int] -> [Int]
fill = until (\x -> length x `rem` 8 == 0) (0:)     
        
-- convert a binary number to an Integer
binToInt :: [Int] -> Integer
binToInt []     = 0
binToInt (x:xs) = 2^length xs * (fromIntegral x) + binToInt xs

-- convert an integer into a string
integerToMessage :: Integer -> String
integerToMessage = binToString.fill.intToBin

-- convert an integer into binary                                  
intToBin :: Integer -> [Int]
intToBin 0 = []
intToBin n | odd n     = intToBin (n `div` 2) ++ [1]
           | otherwise = intToBin (n `div` 2) ++ [0]

-- convert binary to string                
binToString :: [Int] -> String
binToString [] = ""
binToString xs = let (i,rest)  = (take 8 xs, drop 8 xs)    -- char from the list 
                     parseChar = chr.fromIntegral.binToInt -- convert a byte into a Char
                 in parseChar i : binToString rest












   
               
               