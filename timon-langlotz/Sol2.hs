module Sol2 where

import GS
import TAMO

-- Exercise 2.13
test1a = logEquiv1 ((\ p -> not p) (\ p -> not p))
test1b = logEquiv1 (not False) True

--test2 = logEquiv2 id (\ p -> p ==> False) (\ p -> not p)
-- end 2.13

-- Exercise 2.51
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

-- Exercise 2.52
parity :: [Bool] -> Bool
parity xs = mod (count 0 xs) 2 == 0

count :: Int -> [Bool] -> Int
count c []					= c
count c (x:xs)	| x 		= count (c+1) xs
				| otherwise = count c xs
-- end 2.52

-- Exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p as = parity (map p as)
-- end 2.53