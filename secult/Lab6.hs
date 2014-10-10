
module Lab6 (

) where
import Data.List
import System.Random
import Week6

-- exercise 1
-- 3h

exMb :: Integer -> Integer -> Integer -> Integer
exMb x 0 m = 1
exMb x 1 m = x `mod` m
exMb x y m | (y `mod` 2) ==0 = (exMb x (y `div` 2) m) * x^2 `mod` m
           | otherwise = (exMb x (y-1) m) * x `mod` m
