--exercise 2, 40 min
module Solution4

where

import SetOrd
import System.Random
import Data.List
import Test.QuickCheck
import Data.Functor

--exercise 3
getRandomSets :: IO()
getRandomSets = do
	seed <- getStdGen 
	let x = generateRandomSet 10 seed --enter the amount of randoms sets desired
	let y = map list2set x
	putStrLn $ show y
	setStdGen $ snd(next seed)
	
randomList :: Int -> StdGen -> ([Int], StdGen)
randomList n seed = (take n $ randomRs (0, 100) (seed) :: [Int], snd(split seed))

generateRandomSet :: Int -> StdGen -> [[Int]]
generateRandomSet 0 gen = []
generateRandomSet n gen = list : generateRandomSet (n - 1) newGen
		where 
		   (list, newGen) = randomList listSize gen
		   listSize = fst(randomR (0,20) (gen) :: (Int, StdGen))

--quickcheck
-- instance Arbitrary (Set a) where
	-- arbitrary = do 
				-- x <- suchThat (arbitrary :: Gen [a]) (not . null)
				-- return (Set x)
--jonatans solution, since I suck...
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized $ \n ->
        do k <- choose (0,n)
           list2set <$> sequence [arbitrary | _ <- [1..k] ]
	
--tests
noDuplicates :: Set Int -> Bool
noDuplicates (Set x) = length(nub x) == length x

isOrdered :: Set Int -> Bool
isOrdered (Set x) = x == sort x

--exercise 4
setIntersection :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setIntersection (Set []) _ = Set []
setIntersection _ (Set []) = Set []
setIntersection (Set xs) (Set ys) = list2set $ [x | x <- xs, elem x ys]

setUnion :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setUnion (Set xs) (Set ys) = list2set $ xs ++ ys

setDifference :: (Eq a, Ord a) => (Set a) -> (Set a) -> (Set a)
setDifference (Set xs) (Set ys) = list2set $ [x | x <- xs, not $ elem x ys]

--tests
intersectProperty :: (Eq a, Ord a) => (Set a) -> (Set a) -> Bool
intersectProperty x y = subSet intersect x && subSet intersect y
						where intersect = setIntersection x y
						
unionProperty :: (Eq a, Ord a) => (Set a) -> (Set a) -> Bool
unionProperty x y  = subSet x union && subSet y union
					 where union = setUnion x y
					 
differenceProperty :: (Eq a, Ord a) => (Set a) -> (Set a) -> Bool
differenceProperty x y  = subSet difference $ setUnion x y
					 where difference = setDifference x y