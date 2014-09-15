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
kindOfTriangle (x,y,z) | negativewidth || max >= med + low = "Not a triangle" 
                       | x==y && y==z                      = "Equilateral" 
                       | max*max == med*med + low*low      = "Rectangular" 
                       | x==y || x==z || z==y              = "Isoceles" 
                       | otherwise                         = "Other"
                       where
                          negativewidth = x < 0 || y < 0 || z < 0
                          srt = sortTuple (x,y,z)
                          max = thd3 srt
                          med = snd3 srt
                          low = fst3 srt             
               
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | negativewidth || max >= med + low = NoTriangle
               | x==y && y==z                      = Equilateral
               | max*max == med*med + low*low      = Rectangular
               | x==y || x==z || z==y              = Isoceles 
               | otherwise                         = Other
               where
                  negativewidth = x < 0 || y < 0 || z < 0
                  max = thd3 sortTuple (x,y,z)
                  med = snd3 sortTuple (x,y,z)
                  low = fst3 sortTuple (x,y,z)
 
{-
    proof that function triangle is correct:
    Assumption 1: the operators || (inclusive or), >= (bigger or equal), < (strictly smaller than), + (plus), == (equal to), * (multiply) and guards are correctly implemented in Haskell.
    Assumption 2: the sortTuple function returns an ordered triple in ascending order, i.e: (x',y',z') = sortTuple (x,y,z) implies x' <= y' <= z' 
    
    Lemma1: let T be a triangle on a 2d plane, then the length of the sides of T adhere to the property P1 = { x,y,z | x < y + z }, 
            where x is the length of the longest of the triangle sides and y and z are the other two sides.

    Lemma2: Let T be a triangle on a 2d plane, if T has a rectangular angle, then the length of the sides of T adhere to the relation P2 = { x,y,z | x*x = y*y+z*z }, 
            where x is the length of longest of the triangle sides and y and z of the other two sides.
    
    To prove 1: when a triangle can not be formed from the three Input integers, the function returns "Not a triangle"
    
    To prove 2: when a triangle that is Equilateral can be formed, the function returns "Equilateral".
    
    To prove 3: when a triangle can be formed that does not have the properties described in the previous case, but is 
                Rectangular, the function returns "Rectangular".
    
    To prove 4: when a triangle can be formed that does not have the properties described in the previous 2 cases, but 
                is Isoceles, the function returns "Isoceles".
    
    To prove 5: when a triangle can be formed that does not have the properties described in the previous 3 cases, the 
                function returns "Other".
    
    Proof 1:
           - The length of a triangle side can not be negative, 
             so P3 = { x,y,z | x >= 0 && y >= 0 && z >= 0 } 
             which can be negated:
                !P3 = { x,y,z | x < 0 || y < 0 || z < 0} (negation of conjunction)
           
           - According to lemma2 a triangle can only be formed if the lengths of the sides of T adhere the property P1 
             Then a non-Triangle triple should adhere to the negation of P1.
                !P1 = {x,y,z | x >= y + z} (negation of smaller than)
           
           Combined:
           Since the lengths of a triangle have to posess all the properties described above, P4 = {x,y,z | P1 && P3},
           thus P5 = { x,y,z | !P4 } is the set of all length triplets that can not form a triangle.
                P5 = { x,y,z | !p3 || !P1} (negation of conjunction)
                P5 = { x,y,z | x < 0 || y < 0 || z < 0 || x >= y + z  } (filled in P1 and P3)
                
            The first guard of the function checks P5

    Proof 2:
          An equilateral triangle has the property that all it's sides are equal in length, so {x,y,z | x == y, y ==z}
          which is exactly what is checked in the second guard function
    
    Proof 3:
          According to Lemma 2, the set of triangles that are Rectangular has the property {x,y,z | x*x == y*y + z*z}
          This is exactly the property that is checked in the third guard function
    
    Proof 4:
          A triangle that is Isoceles exhibits the following property {x,y,z | x == y || y == z || x == z}.
          This is exactly the property that is checked in the fourth guard function
    
    Proof 5: 
         Since guards are implemented property in Haskell, a non-triangle or a triangle that is equilateral, rectangular 
         or isoceles will be caught before it reaches the last guard, which leaves the case were a triangle can be formed 
         without these properties to the last guard.
               
    Since 

-}
 
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
{-
    no duplicates means that one of these tests is superfluous:
    - all(`elem` ys) xs
    - all(`elem` xs) ys
    
    it also means that list comprehensions are equivalent to set comprehensions
    
    Prop1: isPermutation returns true for each permutation of a list
           let ls be a list of comparable elements, 
           then all elements of this list comprehension should be true:
            [ isPermutation x | x <- Perms ls ]   
-}


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

{-
    number of permutations of a list without duplicates: 
-}


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








