module Lab2 where

import Data.List
import System.Random

-- Estimated time needed: 1:15 hours

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

kindOfTriangle :: (Integer, Integer, Integer) -> String
kindOfTriangle (x,y,z) | negativewidth || max >= med + low = show NoTriangle
                       | x==y && y==z                      = show Equilateral
                       | max^2 == med^2 + low^2            = show Rectangular
                       | x==y || x==z || z==y              = show Isoceles
                       | otherwise                         = show Other
                       where
                          negativewidth = x < 0 || y < 0 || z < 0
                          srt = sortTuple (x,y,z)
                          max = thd3 srt
                          med = snd3 srt
                          low = fst3 srt

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | negativewidth || max >= med + low = NoTriangle
               | x==y && y==z                      = Equilateral
               | max^2 == med^2 + low^2            = Rectangular
               | x==y || x==z || z==y              = Isoceles
               | otherwise                         = Other
               where
                  negativewidth = x < 0 || y < 0 || z < 0
                  max = thd3 (sortTuple (x,y,z))
                  med = snd3 (sortTuple (x,y,z))
                  low = fst3 (sortTuple (x,y,z))

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
randomInput = take 10 $ randomRs (0, 100) $ mkStdGen 66
-- generate testresults
testInput :: [String]
testInput = map show $ sort [(s, x,y,z)  | x <- randomInput,
                                           y <- randomInput,
                                           z <- randomInput,
                                           let s = triangle x y z ]

-- display testresults ordered by shape
doTest :: IO()
doTest = mapM_ putStrLn testInput

-- Exercise 2
-- time spent: 45 minutes
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && equalElemFreq xs ys
                                    
equalElemFreq :: Eq a => [a] -> [a] -> Bool
equalElemFreq ys zs = foldr (\a b -> b && (length (elemIndices a ys) == 
                                           length (elemIndices a zs)))
                            True ys

-- Exercise 3
{-
    This means that we can compute the number of permutations of a list by calculating the factorial.

    Prop1: isPermutation returns true for each permutation of a list
           let ls be a list of comparable elements,
           then all elements of this list comprehension should be true:
            [ isPermutation x | x <- Perms ls ]
    
    But since Prop1 should hold for any list, it is not possible to completely test it.
    
    
-}

canResolveNonEqualLengthsP :: Eq a=>Num a=> ([a]->[a]->Bool) ->  Bool
canResolveNonEqualLengthsP f = not (f [1] [1,2])

canHandleEmptyListsP ::  ([a]->[a]->Bool) ->  Bool
canHandleEmptyListsP f = f [] []

canHandleSingletonsP ::
  Eq a => Num a => ([a]->[a]->Bool) ->  Bool
canHandleSingletonsP f =
  (f [1] [1]) && not (f [1] [2])

canHandleOneSideEmptyListsP ::  Eq a => Num a =>  ([a]->[a]->Bool) ->  Bool
canHandleOneSideEmptyListsP f = (not (f [1] [])) && (not (f [] [2]))

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
tt=[t1,t2,t3,t4,t5,t6,t7]

canHandleOurTestDataP :: ([Integer]->[Integer]->Bool) ->  Bool
canHandleOurTestDataP f = and $ map (\(a,b,c)-> ((f a b) == c )) tt

-- Exercise 4
-- time spent : 30 minutes
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (btween x) (perms xs))

-- receives an element and a list, return a list of list with the original element inserted at each position
btween:: a -> [a] -> [[a]]
btween x []     = [[x]]
btween x (y:ys) = (x:y:ys) : map (y:) (btween x ys)

{-
    number of permutations of a list without duplicates:
    This means that we can compute the number of permutations of a list by calculating the factorial.
-}

-- Another solution which is shorter but not as efficient as the first solution:
perms2 :: Eq a => [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [ x:ys | x <- xs, ys <- perms2 (delete x xs)]

-- Exercise 5
-- time spent : 25 minutes
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = isPermutation x y && (and $ zipWith (/=) x y)

-- Exercise 6
-- time spent : 10 minutes
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) $ perms xs

-- Exercise 7

canHandleEmptyLists ::  ([a]->[a]->Bool) ->  Bool
canHandleEmptyLists f = f [] []

canHandleSingletons ::  Eq a => Num a => ([a]->[a]->Bool) ->  Bool
canHandleSingletons f = not (f [1] [1]) && not (f [1] [2])

canHandleOneSideEmptyLists ::   Eq a => Num a =>  ([a]->[a]->Bool) ->  Bool
canHandleOneSideEmptyLists f = (not (f [1] [])) && (not (f [] [2]))

testData :: [([Integer],[Integer],Bool)]
testData= [
    ([1],[1],False),
    ([2],[1],False),
    ([1,2],[2,1],True),
    ([1,2],[1,3],False),
    ([1..10],(reverse [1..10]) , True),
    ([1..10],(reverse [1..11]) ,False),
    ([1..4],[3,4,1,2],True),
    ([1..4],[4,2,1,3],False),
    ([1,2,3],[1,2],False),
    ([],[1,2],False),
    ([1,2,3],[],False)
    ]

canHandleOurTestData :: ([Integer]->[Integer]->Bool) ->  Bool
canHandleOurTestData f = and $ map (\(a,b,c)-> ((f a b) == c )) testData
--time spent 2 hours * 4 people

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

-- Yes, because the function makes use of my previous function and 
-- picks one element randomly of the result of the previous function without changing it.
-- Since it doesn't change the result, the function will perform as expected.

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

recurrentDerangements :: [Int] -> [Int]
recurrentDerangements = map countDerangements

countDerangements :: Int -> Int
countDerangements 0 = 1
countDerangements 1 = 0
countDerangements n = (n-1)*((countDerangements(n-1)) + (countDerangements(n-2)))
