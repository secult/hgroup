module Lab2
  where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles|Rectangular|Other --incorrect in definition
      deriving (Eq, Show)

programTakingTripleOutputingText :: Num a=>  Ord a=> (a,a,a)->String
programTakingTripleOutputingText (a,b,c) | ((\[x,y,z] -> ((x + y )<= z)) (sort [a,b,c])) = "Not a triangle"
                | (\[x,y,z] -> y==z || x==y) (sort [a,b,c]) = "Isosceles"
                | a==b && b==c &&c==a = "Equilateral"
                | ((\[x,y,z] -> (a^2+b^2==c^2)) (sort [a,b,c])) = "Rectangular"
                | otherwise ="Other"


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c   | ((\[x,y,z] -> ((x + y )<= z)) (sort [a,b,c])) = NoTriangle
                 | ((\[x,y,z] -> (a^2+b^2==c^2)) (sort [a,b,c])) = Rectangular
                 | a==b && b==c &&c==a = Equilateral
                 | (\[x,y,z] -> y==z || x==y) (sort [a,b,c]) = Isosceles

                 | otherwise =Other
-- time spent: 30min
demoData = [(1,2,3,NoTriangle),
            (3,4,5,Rectangular),
            (4,4,5,Isosceles),
            (6,6,6,Equilateral),
            (6,5,9,Other)]


demoCheck :: (Integer,Integer,Integer,Shape)-> Bool
demoCheck (a,b,c,d) = (triangle a b c) == d

runTest= all demoCheck demoData
-- time spent: another 30 min

--task 2
removeFst ::Eq a=> a -> [a]-> [a]
removeFst m [] =[]
removeFst m [a] | m ==a =[]
removeFst m (x:xs) | m ==x =xs
        | otherwise = x:(removeFst m xs)

isPermutation :: Eq a => [a] -> [a] -> Bool
-- too simple and bad for big lists - isPermutation a b = any (==b) (permutations a)
isPermutation [] [] = True
isPermutation xs ys | length xs /=length ys = False
isPermutation xs (y:ys) = isPermutation (removeFst y xs) ys

--task 3

canResolveNonEqualLengths :: Eq a=>Num a=> ([a]->[a]->Bool) ->  Bool
canResolveNonEqualLengths f = not (f [1] [1,2])
--and a lot more

-- I might reduce the amount of posibilities to be tested

--task 4

--task 5

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement a b | not (isPermutation a b) = False
isDerangement (x:xs) (y:ys) = foldl (\a b -> a && uncurry (/=) b) (x/=y) (zip xs ys)
