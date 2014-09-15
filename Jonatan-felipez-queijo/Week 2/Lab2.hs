module Lab2 where 

import Data.List
import System.Random

-- Estimated time needed: one hour

data Shape = NoTriangle 
           | Equilateral 
           | Isoceles 
           | Rectangular 
           | Other deriving (Eq, Ord)

instance Show Shape where
    show s     | s == NoTriangle  = "Not a triangle"
               | s == Equilateral = "Equilateral"
               | s == Rectangular = "Rectangular"
               | s == Isoceles    = "Isoceles"
               | s == Other       = "Other"
           
-- Estimated time needed: one hour
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | max >= med + low             = NoTriangle  -- Triangle check
               | x==y && x==z && z==y         = Equilateral -- All sides are equal
               | max*max == med*med + low*low = Rectangular -- Pythagoras
               | x==y || x==z || z==y         = Isoceles   -- Two sides are qual
               | otherwise                    = Other
               where
                  srt = sortTuple (x,y,z)
                  max = thd3 srt
                  med = snd3 srt
                  low = fst3 srt
  
-- triple (a,b,c) utility functions  
sortTuple :: Ord a => (a, a, a) -> (a, a, a)
sortTuple (x,y,z) = let sorted = sort [x,y,z]
                    in (head sorted, head $ drop 1 sorted, head $ drop 2 sorted)

fst3 (x,_,_) = x
snd3 (_,x,_) = x                    
thd3 (_,_,x) = x
                
---------------------------------------------------- Tests 
-- generate a list of 10 random integers
randomInput :: [Integer]
randomInput = take 10 $ randomRs (0, 1000) $ mkStdGen 66
-- generate testresults            
testInput :: [String] 
testInput = map show $ sort [(s, x,y,z)  | x <- randomInput,
                                           y <- randomInput,
                                           z <- randomInput,
                                           let s = triangle x y z ]
-- display testresults
doTest :: IO()
doTest = mapM_ putStrLn testInput     

-----------------------------------------------------------------------

-- Exercise 2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (`elem` ys) xs

-- Exercise 3


--Testcases: (arg1,arg2,correct result)
-- Testcases empty 
t1 = ([],[], True)
-- Testcase one element         
t2 = ([1],[1], True)
-- Testcase two elements       
t3 = ([1,2], [2,1], True)   
-- Testcase identical lists
t4 = ([1,2,3,4], [1,2,3,4], True)
-- Testcase not a permutation
t5 = ([1,2,3,4],[4,3,2,0],False)
-- Testcase not a permutation, missing one element
t6 = ([1..4],[1..3], False)
-- Testcase not a permutation, completely different list
t7 = ([1..4],[5..8], False)
-- Testcase duplicates, same length
t8 = (1:[1..4],[1..5], False)
-- Testcase duplicates, different length, same elements
t9 = (1:[1..4], [1..4], False)

-- Exercise 4
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (btween x) (perms xs))

-- receives an element and a list, return a list of list with the original element inserted at each position
btween:: a -> [a] -> [[a]]
btween x []     = [[x]]
btween x (y:ys) = (x:y:ys) : map (y:) (btween x ys)

-- test 

-- Exercise 5
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = (length x == length y) && (and $ zipWith (/=) x y)
-- check same length and that same position 
 
-- Exercise 6
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) $ perms xs

-- Exercise 7
-- todo: testable properties and well chosen integer list to test isDerangement

-- Exercise 8
generateDerangement :: Eq a => Int -> [a] -> [a]
generateDerangement i xs =  (deran xs) !! i


data Naam = Jeroen | Bert | Bart | Gert deriving (Eq, Show)
namen = [Jeroen, Bert, Bart, Gert]

getRandomDerangement :: (Eq a, Show a) => [a] -> IO()
getRandomDerangement xs = do 
                         g <- newStdGen                           -- IO operation
                         let num = fst $ randomR (0, length xs) g -- generate random int
                         print $ generateDerangement num xs       -- print random Derangement

-- Yes, because the function makes use of my previous function and picks one of this list. It doesn't change any list at all.                         
             
-- Exercise 9 
{- 
    Suppose that there are n persons numbered 1, 2, ..., n. 
    Let there be n hats also numbered 1, 2, ..., n. 
    We have to find the number of ways in which no one gets the hat 
    having same number as his/her number. 
    Let us assume that the first person takes hat i. 
    There are n − 1 ways for the first person to make such a choice. 
    There are now two possibilities, depending on whether or not 
    person i takes hat 1 in return:

    - Person i does not take the hat 1. 
      This case is equivalent to solving the problem with n − 1 
      persons and n − 1 hats: each of the remaining n − 1 people 
      has precisely 1 forbidden choice from among the remaining n − 1
      hats (i's forbidden choice is hat 1).
    - Person i takes the hat 1. 
      Now the problem reduces to n − 2 persons and n − 2 hats.   

      source: http://en.wikipedia.org/wiki/Derangement
-}

(!!!) :: Integer -> Integer
(!!!) 0 = 1
(!!!) 1 = 0
(!!!) n = (n-1)*(((!!!)(n-1)) + ((!!!)(n-2)))    








