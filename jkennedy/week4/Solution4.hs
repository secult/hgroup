--exercise 2, 40 min
module Solution4

where

import SetOrd
import System.Random
import Data.List
import Test.QuickCheck

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
instance Arbitrary (Set a) where
	arbitrary = list2set $ choose (0, 100)
