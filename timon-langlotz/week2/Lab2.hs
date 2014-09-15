module Lab2 where 

import Data.List
import System.Random

-- 1
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 	| x <= 0 || y <= 0 || z <= 0	= NoTriangle -- length < 0
				| x + y < z || x + z < y || y + z < x = NoTriangle -- triangle inequality
				| x^2 + y^2 == z^2 				= Rectangular
				| x == y && y == z 				= Equilateral
				| x == y || y == z || x == z 	= Isosceles
				| otherwise = Other 

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

-- 2
isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 xs ys = elem ys (permutations xs)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && compareNumberIndices xs xs ys

compareNumberIndices :: Eq a => [a] -> [a] -> [a] -> Bool
compareNumberIndices [] ys zs = True
compareNumberIndices (x:xs) ys zs = length (elemIndices x ys) == length (elemIndices x zs) 
									&& compareNumberIndices xs ys zs


-- 3


-- 4
perms2 :: [a] -> [[a]]
perms2 xs = permutations xs

perms :: [a] -> [[a]]
perms xs = xs : perms' xs 1

perms' :: [a] -> Int -> [[a]]
perms' []     _ 														= [] 
perms' (x:xs) i | i >= factorial (length (x:xs)) 						= []
				| mod i (factorial (length (x:xs))) == length (x:xs)	= (xs ++ [x]) : perms' (xs ++ [x]) (i+1)
				| otherwise 											= (ys ++ [x] ++ zs) : perms' (ys ++ [x] ++ zs) (i+1)
																			where (ys,zs) = splitAt (mod i (length (x:xs))) xs


factorial :: Int -> Int
factorial x | x == 1 = 1
			| otherwise = x * factorial (x-1)
			
			
-- 5
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys

deran :: Eq a => [a] -> [[a]]
deran as = filter' (perms as) as

filter' :: Eq a => [[a]] -> [a] -> [[a]]
filter' [] 	   ys 							= []
filter' (x:xs) ys 	| isDerangement x ys 	= x : (filter' xs ys)
					| otherwise 			= filter' xs ys
