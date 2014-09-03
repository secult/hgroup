module Sol1 where

import GS

maxInt :: [Int] -> Int
maxInt [] = error "empty list" 
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

removeFst :: Int -> [Int] -> [Int]
removeFst _ [] 					= []
removeFst m (x:xs) 	| m == x 	= xs
					| otherwise = x:removeFst xs