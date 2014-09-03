module Sol1 where

import GS

maxInt :: [Int] -> Int
maxInt [] = error "empty list" 
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

removeFst :: Int -> [Int] -> [Int]
removeFst _ [] 					= []
removeFst m (x:xs) 	| m == x 	= xs
					| otherwise = x:removeFst m xs

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

count :: Char -> String -> Int
count _ [] = 0
count c (x:xs) 	| c == x 	= 1 + count c xs
				| otherwise = count c xs
				
				
blowup :: String -> String
blowup = blowup' 1 --alternative: blowup xs = blowup' 1 xs

blowup' :: Int -> String -> String
blowup' _ [] = []
blowup' i (x:xs) = replicate i x ++ blowup' (i+1) xs

--Excercise 1.15
srtString :: [String] -> [String]
srtString [] = []
srtString [x] = [x]
srtString (x:xs) = m : srtString (removeStr m (x:xs))
			where m = findFirstString xs x


findFirstString :: [String] -> String -> String
findFirstString [] s = s
findFirstString (x:xs) s  	| x < s = findFirstString xs x
							| otherwise = findFirstString xs s

removeStr :: String -> [String] -> [String]
removeStr _ [] = []
removeStr y (x:xs) 	| y == x = xs
					| otherwise = x: removeStr y xs
					
--Excercise 1.17 TODO
substring :: String -> String -> Bool
substring (xs) (ys) = prefix (xs) (ys)
substring (x:xs) (y:ys) | ys == (y:ys) && substring xs ys = True
						| otherwise = False
