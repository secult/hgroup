module Lab6 (
) where
import Data.List
import System.Random
import Week6
import Data.Time.Clock
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Control.Exception (evaluate)

-- exercise 1
-- 4h

exMn :: Integer -> Integer -> Integer -> Integer
exMn _ _ 0 = error "division by zero"
exMn x 0 m = (x^0) `mod` m 
exMn x 1 m = x `mod` m
exMn x y m | (y<0) = error "negative exponent"
exMn x y m | (y `mod` 2) ==0 = (exMn x (y `div` 2) m)^2 `mod` m
           | otherwise = (exMn x (y-1) m) * x `mod` m


--exercise 2
-- 3h

-- todo : make compareTime somehow automatized
speed :: IO ()
speed = hspec $ do
  describe "testing speed of exMn in comparisson to expM" $ do
    it "compares speed" $ do
        r<-randomRIO(10000000,90000000):: IO Integer
        print r

        t1<- getCurrentTime
        a <-evaluate $exMn  r 1234357 431
        t2<- getCurrentTime
        d1 <-evaluate$diffUTCTime t2 t1
        t3<- getCurrentTime
        b <-evaluate $ expM r 1234357 431
        t4<- getCurrentTime
        d2 <-evaluate$diffUTCTime t4 t3
        print $(show d1)++ " vs "++(show d2)
        d1<d2 `shouldBe` (True:: Bool)

compareResults :: Integer -> Integer-> Integer-> Bool
--invariant for quickCheck
compareResults a b c | c ==0 = True--only for quickcheck
compareResults a b c | b<0 = True--only for quickcheck
                    | otherwise = (expM (abs a) b (abs c)) == (exMn (abs a) b (abs c))
                        
compareTime :: Integer -> Integer -> Integer -> IO Bool
compareTime a b c = do
        t1<- getCurrentTime
        let x = exMn a b c
        print x
        t2<- getCurrentTime
        let d1 =diffUTCTime t1 t2
        print d1

        t3<- getCurrentTime
        let y =expM a b c
        print y
        t4<- getCurrentTime
        let d2 =diffUTCTime t1 t2
        print d2

        return ((diffUTCTime t1 t2) < (diffUTCTime t3 t4))
--in comparisson,with 10^8 ^10^7 mod 10^3 number  it takes roughly no time against 2s

--exercise 3
--taken from timon, spent too much time on it, day or so
composites :: [Integer]
composites = sieve2 [2..] primes

sieve2 :: [Integer] -> [Integer] -> [Integer]
sieve2 list@(n:ns) (p:ps) = (takeWhile (<p) list) ++
                                if n == p
                                then sieve2 ns ps
                                else sieve2 (dropWhile (<=p) list) ps

 --exercise 4
 --1h
testPrimeF :: Int -> [Integer] -> [(Integer,IO Bool)]
testPrimeF k (x:xs) = (x,(primeF k x)):testPrimeF k xs




showIOList :: [(Integer,IO Bool)] -> IO ()
showIOList ((i,x):xs) = do
                s<-x
                print i
                print s
                showIOList xs

                
 

showFalsePositives :: [(Integer,IO Bool)] -> IO ()
showFalsePositives ((i,x):xs) = do
                s<-x
                if s then do
                    putStrLn $"False positive: "++ (show i)
                    
                else do
                    return ()
                showFalsePositives xs

                
testFalsePositivesFComposites k = do
    let c = testPrimeF k composites
    showFalsePositives c
-- as "k" get higher, more random numbers go through
-- as test repeats k times

 --exercise 5
 --1h
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    isPrime (6*k+1),
    isPrime (12*k+1),
    isPrime (18*k+1) ]

testFalsePositivesFCarmichael k = do
    let c = testPrimeF k carmichael
    showFalsePositives c
{-
Much bigger problem, because the random numbers are not diverse enough
to got the few numbers that carmichael numbers consists of.
Need to get k numbers cca 1000 or so to significantly reduce the amount
 of false positives
-}
--exercise 6
--1h
testPrimeMR :: Int -> [Integer] -> [(Integer,IO Bool)]
testPrimeMR k (x:xs) = (x,(primeMR k x)):testPrimeMR k xs

testFalsePositivesMRCarmichael k = do
    let c = testPrimeMR k carmichael
    showFalsePositives c

testFalsePositivesMRComposites k = do
    let c = testPrimeMR k composites
    showFalsePositives c
--miller -rabin check is more accurate and faster

--exercise 7
--1h but it has wierd results
mersenneCheck :: Integer -> IO Bool
mersenneCheck x = do
                a<- primeMR 3 x
                b<- (primeMR 3 (2^x-1))
                --let c=isPrime x
                --let d=isPrime (2^x-1)
                --e<- primeF 3 x
                --f<- (primeF 3 ((2^x)-1))
                --when (a && b) (print ("MP found "++ (show x)))
                --when (e && f) (print ("F found "++ (show x)))
                --when (c && d) (print ("found "++ (show x)))
                if a&&b 
                then return True
                else return False


mersenneSearch :: [Integer] ->  [(Integer,IO Bool)]
mersenneSearch (x:xs) = (x,mersenneCheck x):mersenneSearch xs


showMersennes :: [(Integer,IO Bool)] -> IO ()
showMersennes   ((i,x):xs) = do
                s<-x
                if s then do
                    putStrLn ("Found mersenne`s prime! 2^"++show i++"-1")
                    return ()
                else do
                    return ()
                if (i `mod`1000)==0 then print i
                else do return ()
                
                showMersennes xs

main start= do
    let c = mersenneSearch [2..]
    showMersennes c
    return ()