module Solution4

where

import Data.Functor
import Data.List
import SetOrd
import System.Random
import Test.Hspec
import Test.QuickCheck

-- exercise 1
-- make something up


-- exercise 2
-- 45 minutes


-- exercise 3
-- 2 hours
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

-- From scratch:                                
getRndSetList :: Int -> (Int, Int) -> StdGen -> ([Set Int], StdGen)
getRndSetList 0 (_,_)   g = ([],g)
getRndSetList n bnd g = let (len,ng)  = randomR bnd (snd $ split g)
                            (set,nng) = getRndSet len ng
                            (sets,fg) = getRndSetList(n-1) bnd nng
                            in (set:sets, fg)

getRndSet :: Int -> StdGen -> (Set Int, StdGen)
getRndSet i g = (list2set $ (randomList i g), snd $ next g)
                
--edited from www.haskell.org/haskelwiki/Examples/Random_list
randomList :: Int -> StdGen -> [Int]
randomList n gen = take n $ unfoldr (Just . randomR(1, maxBound)) gen             
                
-- Using quickcheck
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized $ \n ->
        do k <- choose (0,n)
           list2set <$> sequence [arbitrary | _ <- [1..k] ]
    
-- check for duplicates
propNoDubs :: Set Int -> Bool -- help 
propNoDubs (Set a) = a == nub a

-- check if sorted (for this implementation of set)
propSorted :: Set Int -> Bool
propSorted (Set a) = a == sort a

-- check if after insert of duplicate, the set is the same
propInsert :: Set Int -> Bool
propInsert (Set a) = (foldr insertSet (Set a) a) == (Set a)


-- exercise 4
setIntersection :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setIntersection (Set []) _ = Set []
setIntersection _ (Set []) = Set []
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

-- The difference of two sets are the all the objects that one set has, that the other has not.
-- So the difference has objects that are in either one of the sets, which can be checked by using a union					 
differenceProperty :: (Set Int) -> (Set Int) -> Bool
differenceProperty x y  = subSet difference $ setUnion x y
					 where difference = setDifference x y
                    
-- todo : jonatan testreport! :) (felipez-quark)

                    
-- exercise 5
-- time spent: 20 minutes
type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | r == nr   = sort r
         | otherwise = sort (nub (nr ++ trClos nr))
            where nr = nub (r ++ (r @@ r))

-- exercise 6
-- time spent: 20 minutes
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
            
-- exercise 7
test_trClos xs = testLength xs && testElem xs

-- the length of the transitive closure of a relation should be 
-- at least the same size as the original relation.
testLength :: Set Int -> Bool
testLength xs = length (trClos xs) >= length (nub xs)

-- checks whether the original relation is still included in the
-- transitive closure of the relation.
testElem :: Set Int -> Bool
testElem xs = and (map (\x -> x `elem` (trClos xs)) xs)

-- checks for each pair of element of the pattern (a,b) (b,c) 
-- whether the element (a,c) in included in the relation
-- this function has to be called on a transitive closure and will return true,
-- iff this relation is a transitive closure.
testTrans :: Rel Int -> Bool
testTrans xs =  and $ ([(a,d) `elem` xs | (a,b) <- xs, (c,d) <- xs, b == c, (a,b) `elem` xs, (c,d) `elem` xs])

-- exercise 8
-- Babylonian method
-- No matter what value for x is chosen, the result converges to a
-- f (x0) >= f (x1) >= ... >= f (xn) == sqrt (a)

-- idea: x_(n) and x_(n+1) are the lengths of the site of a rectangle and the two values approach each other 
-- until the rectangle is a square
