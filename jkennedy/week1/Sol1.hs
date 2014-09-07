--Juriaan Kennedy - 6061613
module Sol1 where

import GS

--exercise 1.9
maxInt :: [Int] -> Int
maxInt [] = error "The list is empty"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

--exercise 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst _ [] = []
removeFst m (x:xs) | m == x = xs
		   | otherwise = x : removeFst m xs

--exercise 1.13
count :: Char -> String -> Int
count _ [] = 0
count c (x:xs) | x == c = 1 + count c xs
	       | otherwise = count c xs

--exercise 1.14
blowup :: String -> String
blowup [] = []
blowup xs = blowupMultiply 1 xs

blowupMultiply :: Int -> String -> String
blowupMultiply _ [] = []
blowupMultiply i (x:xs) = replicate (i) x ++ blowupMultiply (i + 1) xs

--exercise 1.15
srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFstString m xs))
		 where m = lowestString xs

removeFstString :: String -> [String] -> [String]
removeFstString _ [] = []
removeFstString str (x:xs) | str == x = xs
			   | otherwise = x : removeFstString str xs

lowestString :: [String] -> String
lowestString [] = []
lowestString [x] = x
lowestString (x:xs) = lowerString x (lowestString xs) 

lowerString :: String -> String -> String
lowerString x y | x <= y = x
		| otherwise = y

--exercise 1.17
substring :: String-> String -> Bool
substring _ [] = False
substring [] _ = True
substring xs (y:ys) | prefix xs (y:ys) = True
		| otherwise = substring xs ys

--exercise 1.20
lengths :: [[a]] -> [Int]
lengths [] = [0]
lengths xs = map length xs

--exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)
