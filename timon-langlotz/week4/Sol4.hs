module Sol4 where

import Data.Functor
import Data.List
import SetOrd
import System.Random
import Test.Hspec
import Test.QuickCheck

-- Exercise 3
-- creates a Set from range (lo,hi) from a list with x elements
createSet :: (Int,Int) -> Int -> IO (Set Int)
createSet (lo,hi) size = do
             seed  <- newStdGen
             let rs = randomlist (lo,hi) size seed
             return $ list2set rs            
 
randomlist :: (Int,Int) -> Int -> StdGen -> [Int]
randomlist (lo,hi) n = take n . unfoldr (Just . randomR (lo,hi))

-- QuickCheck test invariant
testSet xs = testOrderedAndUnique xs && testInsert xs

testOrderedAndUnique :: Set Int -> Bool
testOrderedAndUnique (Set [])     = True
testOrderedAndUnique (Set (x:[])) = True
testOrderedAndUnique (Set (x:xs)) = x < (head xs) && testOrderedAndUnique (Set (xs))

testInsert :: Set Int -> Bool
testInsert (Set xs) = and $ map (\ys -> ys == (Set xs)) (map (\x -> insertSet x (Set xs)) xs)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized $ \n ->
        do k <- choose (0,n)
           list2set <$> sequence [arbitrary | _ <- [1..k] ]
           
-- Exercise 4
intersectSet :: (Eq a, Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) _                        = Set []
intersectSet _ (Set [])                        = Set []
intersectSet (Set (x:xs)) (Set (y:ys)) | x < y = intersectSet (Set xs) (Set(y:ys))
                                       | x > y = intersectSet (Set(x:xs)) (Set ys)
                                       | otherwise = insertSet x (intersectSet (Set xs) (Set ys))

-- unionSet in SetOrd.hs

differenceSet :: (Eq a, Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) xs       = xs
differenceSet xs (Set [])       = xs
differenceSet (Set xs) (Set ys) = unionSet (Set (filter (\x -> x `notElem` xs) ys)) 
                                           (Set (filter (\x -> x `notElem` ys) xs))

operationTest xs ys = unionSet xs ys == unionSet (differenceSet xs ys) (intersectSet xs ys)

-- Exercise 5
type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r | r == nr   = r
         | otherwise = nub (nr ++ trClos nr)
            where nr = nub (r ++ (r @@ r))

-- Exercise 6
main :: IO ()
main = hspec $ do 
    describe "trClos" $ do
        it "returns the transitive closure of the given relation" $
            trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
        it "returns the identity if the given relation has only one element" $
            trClos [(1,2)] `shouldBe` [(1,2)]
--        it "returns an empty relation if the given relation was empty" $
--            trClos [] `shouldBe` []
-- does not work! :(

-- Exercise 7
-- False for second check?! :(
test_trClos xs = length (trClos xs) >= length xs &&
                    and (map (\x -> x `elem` xs) (trClos xs)) &&
                    

-- Exercise 8
-- Babylonian method
-- No matter what value for x is chosen, the result converges to a
-- f (x0) >= f (x1) >= ... >= f (xn) == sqrt (a)

-- idea: x_(n) and x_(n+1) are the lengths of the site of a rectangle and the two values approach each other 
-- until the rectangle is a square


