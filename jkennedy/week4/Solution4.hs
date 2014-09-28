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