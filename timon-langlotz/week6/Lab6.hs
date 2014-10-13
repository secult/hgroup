module Lab6

where
import Data.Char
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
        x <- getStdRandom (randomR (0,1000000000))
        y <- getStdRandom (randomR (0,50000000))
        m <- getStdRandom (randomR (0,50000))
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

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = 
  do n <- getRandomInt maxi
     return [xs !! n]
     where maxi = length xs - 1

-- Exercise 3
composites :: [Integer]
composites = sieve2 [2..] primes

sieve2 :: [Integer] -> [Integer] -> [Integer]
sieve2 list@(n:ns) (p:ps) = (takeWhile (<p) list) ++ 
                                if n == p
                                then sieve2 ns ps
                                else sieve2 (dropWhile (<=p) list) ps

{-
composites' :: (Integer,Integer) -> [Integer]
composites' (lo,hi) = takeDiff [lo..hi] primes hi ++ composites' (hi,2*hi)

takeDiff :: [Integer] -> [Integer] -> Integer -> [Integer]
takeDiff nums primes idx = takeWhile (< cnt) nums \\ takeWhile (< cnt) primes
                                         where cnt = primes !! idx
-}

-- Exercise 4 + 5
-- carmichael test: testFalsePrime #ofSteps carmichael
testFalsePrime :: Int -> [Integer] -> IO ()
testFalsePrime steps (c:cs) = do
                b <- primeF steps c
                if b then 
                    putStrLn $ show $ c
                else
                    testFalsePrime steps cs

-- fermat test: testFalsePrime2 0                 
testFalsePrime2 :: Int -> IO()
testFalsePrime2  steps = do
                b <-  primeF steps 4
                if b then do
                    putStr "4 found as a false positive for the Prime test (" 
                    putStr $ show $ steps
                    putStr " x tested)"
                    putStrLn ""
                    testFalsePrime2 (steps+1)
                else 
                    testFalsePrime2 steps
               
{-- 
For [1..20] steps I could find 4 to be a false positive in the primeF test in reasonable time.
--}

-- Exercise 6

testFalseMRPrime:: Int -> IO ()
testFalseMRPrime  steps = do
                let num = head carmichael
                b <-  primeMR steps num
                if b then do
                    putStr $ show num
                    putStr " found as a false positive for the Prime test (" 
                    putStr $ show $ steps
                    putStr " x tested)"
                    putStrLn ""
                    testFalseMRPrime (steps+1)
                else 
                    testFalseMRPrime steps

{-- 
For [1..5] steps I could find 294409 to be a false positive in the primeMR test in reasonable time.
 --}

-- Exercise 7
--mersennePrime :: IO Integer
mersennePrime = do
                num <- getRandomInt 200
                let p = primes !! num
                --print p
                let newP = (2^p) - 1
                isPrime <- primeMR 5 newP
                if (isPrime) then
                    print newP
                else
                    mersennePrime
                    
-- Exercise 8
-- interactive en-/decryption with, keys have specified bitlength
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

primeWithBits :: Int -> IO Integer
primeWithBits bits = do
                     x <- getStdRandom (randomR (0,2^bits -1))
                     if odd x then do
                        isPrime <- primeMR 5 x
                        if isPrime then
                            return x
                        else
                            primeWithBits bits
                     else
                        primeWithBits bits


messageToInteger :: String -> Integer
messageToInteger s = binToInt $ concat $ map charToBin s

charToBin :: Char -> [Int]
charToBin c = fill $ reverse $ decToBin $ ord c
                     where
                        decToBin 0 = []
                        decToBin y = let (a,b) = quotRem y 2 in [b] ++ decToBin a

fill :: [Int] -> [Int]
fill xs | (length xs) `mod` 8 == 0 = xs
         | otherwise                          = fill (0:xs)
  
binToInt :: [Int] -> Integer
binToInt []       = 0
binToInt (x:xs) = 2^(length (x:xs) -1) * (fromIntegral x) + binToInt xs


integerToMessage :: Integer -> String
integerToMessage x = binToString binRep
                                 where binRep = fill $ intToBin x

intToBin :: Integer -> [Int]
intToBin 0 = []
intToBin n | n `mod` 2 == 1 = intToBin (n `div` 2) ++ [1]
                | n `mod` 2 == 0 = intToBin (n `div` 2) ++ [0]
                
binToString :: [Int] -> String
binToString [] = ""
binToString xs = (chr $ fromIntegral $ binToInt $ take 8 xs) : binToString (drop 8 xs)