module Tutorial_QuickCheck where

import Test.QuickCheck
import System.Random

sumIsEven :: [Int] -> Bool
sumIsEven xs = mod ((sum xs)) 2 == 0

doubleEven xs = sumIsEven xs 

