module Sol1 where

import Prelude
import GS

-- Exercise 1.9 Define a function that gives the maximum of a list of integer, using the max function
maxInt :: [Int] -> Int 
maxInt []     = error "empty list..."
maxInt (x:xs) = maxInt' xs x
    where maxInList' [a] b    = a `max` b
          maxInList' (x:xs) a = maxInList' xs (a `max` x)   


          
          
-- Exercise 1.10 Define a function removeFst that removes the first occurrence of an integer in a list
removeFst :: Int -> [Int] -> [Int]
removeFst _ [] = []
removeFst y (x:xs) | y == x    = xs
                   | otherwise = x: removeFst y xs
                  
-- Excercise 1.13 Function for counting number of occurrences of a character in a string 
count :: Char -> String -> Int 
count _ []      = 0
count c (x:xs)  | x == c    = 1 + count c xs
                | otherwise =     count c xs
                            
-- Exercise 1.14 Function blowup "a1a2a3" -> "a1a2a2a3a3a3"
blowup :: String -> String
blowup = blowup' 1 
    where blowup' _ []     =  []
          blowup' i (x:xs) = (replicate i x) ++ blowup' (i+1) xs   


          
-- Exercise 1.15 sort string in alphabetical order
srtString :: [String] -> [String]
srtString []     = []
srtString ls = min : srtString( rmFst min ls) 
    where min = minInList ls

rmFst :: String -> [String] -> [String]
rmFst _ []     = []
rmFst s (x:xs) | x == s = xs
               | otherwise = x: rmFst s xs
       
minInList :: [String] -> String
minInList [x] = x
minInList (x:xs) = minInList' x xs
    where minInList' y [x]    = y `min` x
          minInList' y (x:xs) = minInList' (y `min` x) xs
          
-- Exercise 1.17 Function that determins if x is a substring of y
substring :: String -> String -> Bool 
substring [] []      = True
substring _ []       = False
substring sub (x:xs) = prefix sub (x:xs) || substring sub xs

-- Exercise 1.20 lengths of elements in list
lengths :: [[a]] -> [Int]
lengths = map length

-- Exercise 1.21 lengths of elements in list
sumLengths :: [[a]] -> Int
sumLengths = sum.lengths




          
