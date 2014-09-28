module Sol4 where

import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

-- creates a Set from range (lo,hi) from a list with x elements
createSet :: (Int,Int) -> Int -> IO (Set Int)
createSet (lo,hi) size = do
             seed  <- newStdGen
             let rs = randomlist (lo,hi) size seed
             return $ list2set rs            
 
randomlist :: (Int,Int) -> Int -> StdGen -> [Int]
randomlist (lo,hi) n = take n . unfoldr (Just . randomR (lo,hi))

testOrderedAndUnique :: Set Int -> Bool
testOrderedAndUnique (Set [])     = True
testOrderedAndUnique (Set (x:[])) = True
testOrderedAndUnique (Set (x:xs)) = x < (head xs) && testOrderedAndUnique (Set (xs))

instance Arbitrary (Set Int) where 
    arbitrary = oneof [return 0, return 1]

