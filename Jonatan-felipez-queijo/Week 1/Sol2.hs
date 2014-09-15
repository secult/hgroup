module Sol2 where 

import GS
import TAMO

-- Tip for my team mates: 
-- In the functions below, I tend to use the '$' operator. 
-- This is a low priority operator which I use to write less parentheses
-- 'foo $ bar' is the same as 'foo (bar)' in the sense that 'bar' has 
-- priority over 'foo'

-- Exercise 2.13 Implement checks for the principles from Theorem 2.12
theorem212n1a  = lequiv (not True) (False)             
--todo 
theorem212n2  = lequiv (\p -> p ==> False) (\p -> not p)
theorem212n3a = lequiv (\p -> p || True) (\_ -> True)
theorem212n3b = lequiv (\p -> p && False) (\_ -> False)
theorem212n4a = lequiv (\p -> p || False) id
theorem212n4b = lequiv (\p -> p && True) id
theorem212n5  = lequiv (\p -> p || not p) (\_ -> True)
theorem212n6  = lequiv (\p -> p && not p) (\_ -> False)

--Exercise 2.15 Write Haskell definitions of contraiction tests for prop. func. with 1,2,3 vars
-- I know parentheses are unnecessary, but I find it's more clear 
-- what parameter the function expects if i use them

contrad1:: (Bool -> Bool) -> Bool
contrad1 bf = and [not(bf x) | x <- [True, False]]

contrad2:: (Bool -> Bool -> Bool) -> Bool            
contrad2 bf = and [not(bf x y) | x <- [True, False], 
                                 y <- [True, False]]

contrad3:: (Bool -> Bool -> Bool) -> Bool
contrad3 bf = and [not(bf x y) | x <- [True, False], 
                                 y <- [True, False], 
                                 z <- [True, False]]
                                 
-- 2.51 Implement function unique true for unique p xs if one object satisfies p
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = xor $ map p xs

-- exclusive or of list
xor :: [Bool] -> Bool
xor [] = False
xor (x:xs) | x         = and $ map not xs
           | otherwise = xor xs 

-- 2.52 parity: 
parity :: [Bool] -> Bool 
parity xs = let truths = filter id xs 
            in (length truths) `mod` 2 == 0 
        
-- 2.53 evenNR: true if even number of xs have property p
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity $ map p xs







                                 
                                 