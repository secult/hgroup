module Solution4

where

import Data.Functor
import Data.List
import SetOrd
import System.Random
import Test.Hspec
import Test.QuickCheck


{----------------------------------------------------------------------------------------------------------------

    Exercise 1
    Time Spent: 2 hours

----------------------------------------------------------------------------------------------------------------}
 
{-
Jonatan: 
For some reason, the logics course on my uni didn't cover relations o.0 (or at least not that I can remember). 
So that's why I felt the jump in difficulty last week in Testing, I had already seen al we covered up until now. 
That said, I think relations should be fine now that I studied the Haskell Road chapter properly.
Exercises in the Haskell Road I had difficulties with:
- Applying the concept of closure to the differnt types of relations (I've practiced this a bit on practical examples, so this should be fine now)
- 5.36, the difficulty here was in generalizing the proof, but with help of Pavol I got it solved
- 5.39 2, We solved this in the workshop, but before this the trouble I had was deciding on what S should look like (what properties it should have)
- 5.48 2, I do not get what the meaning is of "extends" in this exercise.

A general question: I wonder which libraries we may use when solving the assignments. E.g. We use Data.Functor here because it provides an easy syntax 
for making Set a an instance of Arbitrary (although an Monad extract and return would also work fine). We also considered using the union, intersect 
and difference functions from Data.List for exercise 4, but since we're working with relations we think it's cooler to use list comprehensions. 
But suppose we used the functions from Data.List, would this be a problem?

Timon: 
What I am struggling most with is monads and random values. I am used to the concept of Object oriented programming
languages where it is no problem for a method to just return some random values. In Haskell, this is quite difficult
for me since I always have to ensure that I am not mixing up IO and other data types.
I also didn't know how to use list comprehension, but this issue is fine now. 
The other topics from the book I am able to manage.
-}

{----------------------------------------------------------------------------------------------------------------

    Exercise 2
    Time Spent: 45 minutes
    
----------------------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------------------

   Exercise 3
   Time Spent: 2:30 hours

----------------------------------------------------------------------------------------------------------------}   
-- From scratch: 
interactiveRandomSetGenerator :: IO()
interactiveRandomSetGenerator = do
                                    seed <- newStdGen
                                    putStrLn "How many Sets?"
                                    i1 <- getLine
                                    let n = read i1 :: Int
                                    
                                    putStrLn "Min Num of elem in a set?"
                                    i2 <- getLine
                                    let lo = read i1 :: Int 
                                    
                                    putStrLn "Max num of elem in a set?"
                                    i3 <- getLine
                                    let hi = read i3 :: Int
                                    
                                    let result = fst $ getRndSetList n (lo,hi) seed 
                                    mapM_ putStrLn (map show result) 
                                    putStrLn "Happy now? (y/n)"
                                    i4 <- getLine
                                    if i4 == "n" 
                                    then interactiveRandomSetGenerator
                                    else return ()                                            
                                                                                     
getRndSetList :: Int -> (Int, Int) -> StdGen -> ([Set Int], StdGen)
getRndSetList 0 (_,_)   g = ([],g)
getRndSetList n bnd g = let (len,ng)  = randomR bnd (snd $ split g)
                            (set,nng) = getRndSet len ng
                            (sets,fg) = getRndSetList(n-1) bnd nng
                            in (set:sets, fg)

getRndSet :: Int -> StdGen -> (Set Int, StdGen)
getRndSet i g = (list2set $ (randomList i g), snd $ next g)
                
-- edited from www.haskell.org/haskelwiki/Examples/Random_list
randomList :: Int -> StdGen -> [Int]
randomList n gen = take n $ unfoldr (Just . randomR(1, maxBound)) gen             
                
-- Using quickcheck
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized $ \n ->
        do k <- choose (0,n)
           list2set <$> sequence [arbitrary | _ <- [1..k] ] -- <$> maps a function over a functor 
    
-- check for duplicates
propNoDubs :: Set Int -> Bool -- help 
propNoDubs (Set a) = a == nub a

-- check if sorted (for this implementation of set)
propSorted :: Set Int -> Bool
propSorted (Set a) = a == sort a

-- check if after insert of duplicate, the set is the same
propInsert :: Set Int -> Bool
propInsert (Set a) = (foldr insertSet (Set a) a) == (Set a)

{----------------------------------------------------------------------------------------------------------------

    Exercise 4
    Time spent: 1,5 hour
    
----------------------------------------------------------------------------------------------------------------}

setIntersection :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setIntersection (Set xs) (Set ys) = list2set $ [x | x <- xs, elem x ys]

setUnion :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setUnion = unionSet -- given in SetOrd

setDifference :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setDifference (Set xs) (Set ys) = list2set $ [x | x <- xs, not $ elem x ys]

-- test properties

-- The intersection of two sets are all the objects that those sets share. 
-- This means an intersection is a subset of both seperate sets
intersectProperty :: (Set Int) -> (Set Int) -> Bool
intersectProperty x y = subSet intersect x && subSet intersect y
						where intersect = setIntersection x y

-- A union of two sets contains all the objects of those sets.
-- So both sets are a subset of their union.				
unionProperty :: (Set Int) -> (Set Int) -> Bool
unionProperty x y  = subSet x union && subSet y union
					 where union = setUnion x y

-- The difference of two sets are all the objects that one set has, that the other has not.
-- So the difference has objects that are in either one of the sets, which can be checked by using a union					 
differenceProperty :: (Set Int) -> (Set Int) -> Bool
differenceProperty x y  = subSet difference $ setUnion x y
					 where difference = setDifference x y

{--
    Test Report
    
    We tested the functions using the properties above.
    
    Using own random data generator:
    we used testSetOpProps (see below) to test 100 random testCases of sets that have between 0 and 50 elements
    testSetOpProps returns a testcase if the test came out false, but is not as fancy as quickcheck in that it shrinks the testcase
    
    Using quickcheck:
    We made the Set a an instance of Arbitrary in the previous exercises, but quickcheck needs a hint as to which datatype it needs 
    to generate Sets of so we decided on Int (otherwise only empty sets will be tested, because these are polymorphic due to []).
    We use verbosecheck to convince ourself that relevant testdata was generated.

--}

printFalseTest :: Show a => ((Set a, Set a), Bool) -> IO ()
printFalseTest (a,False) = putStrLn $ "false: " ++ (show a)
printFalseTest _         = return () 

-- return the testcase and the result of the test
doTest2 :: (a -> a -> Bool) -> (a,a) ->((a,a),Bool) 
doTest2 f (a,b) = ((a,b),f a b)  
            
testSetOpProps :: IO ()
testSetOpProps = do
                   -- generate testdata
                   seed <- newStdGen
                   let n = 100
                   let lo = 0
                   let hi = 50
                   let tests = getRndSetList n (lo,hi) seed
                   let testcases1 = fst $ tests
                   let testcases2 = fst $ getRndSetList n (lo,hi) (snd tests)
                   let zipTests = zip testcases1 testcases2
                   -- test out properties
                   putStrLn "----------------- intersectProperty"
                   mapM_ printFalseTest (map (doTest2 intersectProperty) zipTests)
                   putStrLn "----------------- unionProperty"
                   mapM_ printFalseTest (map (doTest2 unionProperty) zipTests)
                   putStrLn "----------------- differenceProperty"
                   mapM_ printFalseTest (map (doTest2 differenceProperty) zipTests)
                   putStrLn "----------------- Tests Done!" 
                   return ()                     
                    
{---------------------------------------------------------------------------------------------------------------- 

    Exercise 5
    Time spent: 20 minutes
    
----------------------------------------------------------------------------------------------------------------}
type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | r == nr   = sort r
         | otherwise = sort (nub (nr ++ trClos nr))
            where nr = nub (r ++ (r @@ r))

{----------------------------------------------------------------------------------------------------------------

   Exercise 6
   Time spent: 20 minutes

----------------------------------------------------------------------------------------------------------------}
main :: IO ()
main = hspec $ do 
    describe "trClos" $ do
        it "returns the transitive closure of the given relation" $
            trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
        it "returns the identity if the given relation has only one element" $
            trClos [(1,2)] `shouldBe` [(1,2)]
        it "returns an empty relation if the given relation was empty" $
            trClos ([] :: Rel Int) `shouldBe` ([] :: Rel Int)
        it "returns a relation without duplicates if there are any" $
            trClos [(-7,7),(-7,7)] `shouldBe` [(-7,7)]

{----------------------------------------------------------------------------------------------------------------
            
   Exercise 7
   Time spent: 40 min

----------------------------------------------------------------------------------------------------------------}   

-- Combine the properties to use more easily in quickCheck
test_trClos xs = testLength xs && testElem xs && testTrans (trClos xs)

-- the length of the transitive closure of a relation should be 
-- at least the same size as the original relation.
testLength :: Rel Int -> Bool
testLength xs = length (trClos xs) >= length (nub xs)

-- checks whether the original relation is still included in the
-- transitive closure of the relation.
testElem :: Rel Int -> Bool
testElem xs = and (map (\x -> x `elem` (trClos xs)) xs)

-- checks for each pair of element of the pattern (a,b) (b,c) 
-- whether the element (a,c) in included in the relation
-- this function has to be called on a transitive closure and will return True,
-- iff this relation is a transitive.
testTrans :: Rel Int -> Bool
testTrans xs =  and $ ([(a,d) `elem` xs | (a,b) <- xs, (c,d) <- xs, b == c])

{----------------------------------------------------------------------------------------------------------------

   Exercise 8
   Time spent: 40 min
   
----------------------------------------------------------------------------------------------------------------}
{- 

Babylonian method:
No matter what value for x is chosen, the result converges to a (of course, better start values for x decrease
the amount of required steps.
f (x0) >= f (x1) >= ... >= f (xn) == sqrt (a)

Idea behind this algorithm: x_(n) can be seen as one site of a rectangle, where the area of the rectangle is A (a²). 
The goal of the algorithm now is to create a square with the given area A by recursion (exit condition x == f(x)).
The algorithm uses x_(n) as a start value to calculate x_(n+1), where x_(n+1) is the second page of the rectangle.
Two opposing sites have a side length of x_(n) and the two others have a length of x_(n+1). The area of the 
rectangle stays the same for each step, but the side lenghts change until the rectangle is a square.(x_(n) == x_(n+1))

-}