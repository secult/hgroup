module Sol2 where

import GS
import TAMO

-- Excercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p []		= False
unique p (a:as) = oneTrue (map p (a:as)) False

oneTrue :: [Bool] -> Bool -> Bool 
oneTrue [] b 	| b = True
				| otherwise = False
oneTrue (x:xs) True | x = False
					| otherwise = oneTrue xs True
oneTrue (x:xs) False 	| x = oneTrue xs True
						| otherwise = oneTrue xs False
-- end 2.51

-- Excercise 2.52
parity :: [Bool] -> Bool
parity xs = mod (count 0 xs) 2 == 0

count :: Int -> [Bool] -> Int
count c []					= c
count c (x:xs)	| x 		= count (c+1) xs
				| otherwise = count c xs
-- end 2.52

-- Excercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p as = parity (map p as)
-- end 2.53