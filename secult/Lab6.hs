module Lab6 (
) where
import Data.List
import System.Random
import Week6
import Data.Time.Clock
import Test.Hspec
import Test.QuickCheck
import System.IO.Unsafe

-- exercise 1
-- 3h

exMn :: Integer -> Integer -> Integer -> Integer
exMn _ _ 0 = error "division by zero"

exMn x 0 m = 1
exMn x 1 m = x `mod` m
exMn x y m | (y `mod` 2) ==0 = ((exMn x (y `div` 2) m) * x)^2 `mod` m
           | otherwise = (exMn x (y-1) m) * x `mod` m


--exercise 2
-- 2h
--results are fine
-- todo : make compareTime somehow automatized
speed :: IO ()
speed = hspec $ do
  describe "testing speed of exM in comparisson to expM" $ do
    it "compares speed" $ do
        t1<- getCurrentTime
        let a = expM 12345678 12345 431
        t2<- getCurrentTime

        t3<- getCurrentTime
        let b =exMn 12345678 12345 431
        t4<- getCurrentTime

        diffUTCTime t1 t2 > (diffUTCTime t1 t2) `shouldBe` (True:: Bool)

compareResults :: Integer -> Integer-> Integer-> Bool
--invariant for quickCheck
compareResults a b c = expM a_  b_ c_ == exM a_ b_ c_
                        where a_ = abs a
                              b_ = abs b
                              c_ = (abs c)+1

compareTime :: Integer -> Integer -> Integer -> IO Bool
compareTime a b c = do
        t1<- getCurrentTime
        let x = exMn a b c
        print x
        t2<- getCurrentTime

        t3<- getCurrentTime
        let y =expM a b c
        print y
        t4<- getCurrentTime

        return ((diffUTCTime t1 t2) < (diffUTCTime t3 t4))

--exercise 3
--taken from timon, spent too much time on it
composites :: [Integer]
composites = sieve2 [2..] primes

sieve2 :: [Integer] -> [Integer] -> [Integer]
sieve2 list@(n:ns) (p:ps) = (takeWhile (<p) list) ++
                                if n == p
                                then sieve2 ns ps
                                else sieve2 (dropWhile (<=p) list) ps

 --exercise 4
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
                    print ("False positive",i)
                    
                else do
                    return ()
                showFalsePositives xs

                
testFalsePositiveComposite k = do
    let c = testPrimeF k composites
    showFalsePositives c
-- Fermat’s Little Theorem is utilized by primeF
-- as "k" get higher, more random numbers go through exponentiation
-- as test repeats k times
--i am at 556115 now with k=5, 488647 with k=10
 --exercise 5
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    isPrime (6*k+1),
    isPrime (12*k+1),
    isPrime (18*k+1) ]

testFalsePositiveCarmichael k = do
    let c = testPrimeF k carmichael
    showFalsePositives c
--found only 2098397876980204801
--exercise 6

testPrimeMR :: Int -> [Integer] -> [(Integer,IO Bool)]
testPrimeMR k (x:xs) = (x,(primeMR k x)):testPrimeMR k xs

testFalsePositiveMRcarmichael k = do
    let c = testPrimeMR k carmichael
    showFalsePositives c

testFalsePositiveMRcomposites k = do
    let c = testPrimeMR k composites
    showFalsePositives c
--i can see that from k=2 miller -rabin check is more accurate and 
--has less false positives, but only in large numbers
--

--exercise 7

mersenneCheck :: Integer -> IO Bool
mersenneCheck x = do
                a<- primeMR 3 x
                b<- (primeMR 3 (2^x-1))
                if a && b
                then return True
                else return False


mersenneSearch :: [Integer] ->  [(Integer,IO Bool)]
mersenneSearch (x:xs) = (x,mersenneCheck x):mersenneSearch xs


showMersennes :: [(Integer,IO Bool)] -> IO ()
showMersennes   ((i,x):xs) = do
                s<-x
                if s then do
                    print ("Found mersenne! 2^",i,"-1")
                else do
                    return ()
                showMersennes xs

main = do
    let c = mersenneSearch [3..]
    showMersennes c
    --showIOList c
    return ()