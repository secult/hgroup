module Lab4 where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List
import SetOrd
import System.Random


fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)



--exercise 1
--5.12 what do you mean by "show"


--exercise 2
isEven :: Int-> Bool

isEven a | a==52= False
                |even a = True
                | a==193 = True
                | a==0 = error "this is zero"
                | otherwise = False

main :: IO ()
main = hspec $ do
  describe "testing isEven" $ do
    it "returns False if odd number" $ do
      (nub $ map isEven [1,3..9999]) `shouldBe` ([False]::[Bool])

    it "returns True if even number" $
      (nub $ map isEven [2,4..10000]) `shouldBe` ([True] ::[Bool])

    it "returns True if big even number" $
      (nub $ map isEven [200,202..10000]) `shouldBe` ([True] ::[Bool])

    it "throws an exception if used 0" $ do
      evaluate (isEven 0) `shouldThrow` anyErrorCall


--exercise 2- time spent: few hours

--exercise 3

randomSetGenerator :: Int -> Set Int ->IO (Set Int)
randomSetGenerator 0 set = return set
randomSetGenerator x (Set s) = do
                                v <- getStdRandom( randomR (0,2^31))
                                set <- randomSetGenerator (x-1) (Set (v:s))
                                return set
{-
ok you have the point, how after i made the test in quickcheck i see
that i should make a List generator and make set from it afterwards, and check
not the generator, but the precondition(list) and
 postcondition(ordered list without duplicates)
-}


--time spent 1h45min

--invariant- if the set is a ordered list without duplicates
setIsWithoutDuplicatesAndOrdered :: [Int] -> Bool
setIsWithoutDuplicatesAndOrdered x = (\(Set s) -> s == (sort.nub $x)) (list2set x)

runTest2 = quickCheck setIsWithoutDuplicatesAndOrdered
--time spent 20m
