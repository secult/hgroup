module Lab2 where 

import Data.List
import System.Random

--exercise 2, 15 min
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem xs (permutations ys)

--exercise 3, 10 min
check1 = (isPermutation [1] [1]) == True
check2 = isPermutation [2] [1] == False
check3 = isPermutation [1,2] [2,1] == True
check4 = isPermutation [1,2] [1,3] == False
check5 = isPermutation [1..10] (reverse [1..10]) == True
check6 = isPermutation [1..10] (reverse [1..11]) == False

--exercise 4, 25 min
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:ys | x <- xs, ys <- permutations (delete x xs)]

--the number of permutations are equal to the factorial of the length of the input list.
--knowing this you can always test if the number of permutations is correct

--exercise 5, 30 min
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && derangementPositions xs ys

derangementPositions :: Eq a => [a] -> [a] -> Bool
derangementPositions [] _ = True
derangementPositions (x:xs) (y:ys) = x /= y && derangementPositions xs ys

--exercise 6, 5 min
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (perms xs)

--exercise 7, 10 min
checkDerangement1 = isDerangement [1] [1] == False
checkDerangement2 = isDerangement [2] [1] == False
checkDerangement3 = isDerangement [1,2] [2,1] == True
checkDerangement4 = isDerangement [1,2] [1,3] == False
checkDerangement5 = isDerangement [1..10] (reverse [1..10]) == True
checkDerangement6 = isDerangement [1..10] (reverse [1..11]) == False
checkDerangement7 = isDerangement [1,2,3,4] [3,4,1,2] == True
checkDerangement8 = isDerangement [1,2,3,4] [4,2,1,3] == False

--exercise 8, 30 min, how the fuck do you use IO... Anways because i used the same functions, this function should perform as expected
-- sinterklaasRandom :: Eq a => [a] -> [a]
-- sinterklaasRandom xs = derangements !! getStdRandom (randomR (1, (length derangements)))
--					where derangements = deran xs
					
--exercise 9, 30 min
recurrentDerangements :: Eq a => [a] -> Int
recurrentDerangements xs = countDerangements (length xs)

countDerangements :: Int -> Int
countDerangements 0 = 1
countDerangements 1 = 0
countDerangements n = (n - 1) * (countDerangements (n - 1) + countDerangements (n - 2))