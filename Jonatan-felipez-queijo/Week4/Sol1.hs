module Sol1 where

import SetOrd
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random
import Data.List
import Data.Functor
        
        
{-------------------------------------------------------------------------------------------------------------

    Exercise 3: Random data generator

--------------------------------------------------------------------------------------------------------------}
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

 
{-------------------------------------------------------------------------------------------------------------

    Exercise 4: Set intersection, set union, set difference, random checking

--------------------------------------------------------------------------------------------------------------}      

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = list2set $ intersect a b 

--setUnion ycontrib xcontrib
                           --where 
                              --ycontrib = foldr (\a b -> if a `elem` x then insertSet a b else b) emptySet y
                              --xcontrib = foldr (\a b -> if a `elem` y then insertSet a b else b) emptySet x                            
                              
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a)(Set b) = list2set $ union a b--foldr insertSet b a

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set x) (Set y) = list2set $ x \\ y 


                            -- setUnion ycontrib xcontrib
                            --where 
                              --ycontrib = foldr (\a b -> if a `elem` x then b else insertSet a b) emptySet y
                              --xcontrib = foldr (\a b -> if a `elem` y then b else insertSet a b) emptySet x                            

-- Union properties                             
propUnion1 :: Set Int -> Set Int -> Bool
propUnion1 a b = forall a (`inSet` union) && forall b (`inSet` union)                              
               where union = setUnion a b

propUnion2 :: Set Int -> Set Int -> Bool
propUnion2 a b = forall (setUnion a b) (\x -> x `inSet` a || x `inSet` b)          
               
-- Intersection properties           
propIntersection1 :: Set Int -> Set Int -> Bool
propIntersection1 a b = forall res (`inSet` a) && forall res (`inSet` b) 
                    where res = setIntersection a b     
                    
-- Dfference properties
propDifference1 :: Set Int -> Set Int -> Bool
propDifference1 a b = p1 && p2
                    where 
                        res = setDifference a b
                        p1 = forall res (\x ->if x `inSet` a 
                                              then not(x `inSet` b) 
                                              else not(x `inSet` a) )
                        p2 = forall res (\x ->if x `inSet` b 
                                              then not(x `inSet` a) 
                                              else not(x `inSet` b))
-- Helpers
forall :: Set a -> (a -> Bool) -> Bool 
forall (Set a) f = all f a
                              
{-------------------------------------------------------------------------------------------------------------

    Exercise 5: Transitive closure

--------------------------------------------------------------------------------------------------------------}

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [(x,y) | (x,y) <- r, (w,z) <- s, y == w]
       
--trClos :: Ord a => Rel a -> Rel a
--trClos a = tr  
 --       where connectors = a@@a
        
pairsThatBeginWith :: Ord a => a -> Rel a -> Rel a
pairsThatBeginWith a = filter (\z -> fst z == a )
   
        
--expand :: Ord a => Rel a -> Rel (a,a)
--expand orig = [((a,b),(w,z)) | (a,b) <- orig@@orig, (w,z) <- orig, b == w] 

--clos1 :: Eq a => Rel a -> Rel a -> Rel a
--clos1 a b = undefined --[(x,y)| (x,y) <- a, (w,z) <- b, (q,p) <-    ] 
         --where neighbours = a @@ b

trClos :: Ord a => Rel a -> [a,(Rel a)]
trClos a = map (\a -> ) (a@@a)
        where conn = a@@a
{-------------------------------------------------------------------------------------------------------------

    Exercise 6: Hspec of transitive closure function

--------------------------------------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------------------------------------

    Exercise 7: Testing of transitive closure function

--------------------------------------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------------------------------------

    Exercise 8: Bonus
    
    expl:
    first the fp:
    - it is a function that gets a function and a float as an argument
    - it checks if the float and the function applied on the float is the same result
        - If this is the case, then the guess is good enough
        - otherwise apply the function to the float again.
    
    basically the function recurses until the recursion doesn't find a new value (fix-point finder)
    
    then the approximation function:
        (\x -> (x + a/x)/2), (\x -> (x/2) + (a/x)/2)
        
        - here x is the argument that is being recursed
        - here a is the original argument
    
    since the square root is never more than half of the (original number + 1)
        - At small numbers (<= 3) +1 dominates
        - At bigger numbers (> 3) half of the original number dominates 

    the function first tries the (x + x/x)/2 (this equals: (x + 1)/2), which is a guess that is most likely too high.
    if it's not too high, you have found the square root
    if it is too high, recurse to find a better estimate by trying:
        (x + 1)/4 + x/(x+1)/4
        - this function will have an upper bound of (x+1/2), because the terms are divided by 4 instead of two
        - this function will have a lower bound of the square root of x, because....
        
--------------------------------------------------------------------------------------------------------------}


