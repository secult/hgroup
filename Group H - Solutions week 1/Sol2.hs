module Sol2 where

import GS
import TAMO

--exercise 2.13
check1a = not True <=> False 
check1b = not False <=> True
check2  = lequiv (\ p -> p ==> False) (\ p -> not p)
check3a = lequiv (\ p -> p || True) (\ _ -> True)
check3b = lequiv (\ p -> p && False) (\ _ -> False)
check4a = lequiv (\ p -> p || False) id
check4b = lequiv  (\ p -> p && True) id
check5  = lequiv  (\ p -> p || not p) (\ _ -> True)
check6  = lequiv  (\p -> p && not p) (\ _ -> False)

--exercise 2.15
propContradiction1 :: (Bool -> Bool) -> Bool
propContradiction1 bf = not(bf True) && not (bf False)

propContradiction2 :: (Bool -> Bool -> Bool) -> Bool
propContradiction2 bf = and [ not (bf p q) | p <- [True,False], 
                                             q <- [True,False]]
							 
propContradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
propContradiction3 bf = and [ not (bf p q r) | p <- [True,False], 
                                               q <- [True,False],
                                               r <- [True, False]]
                                     
--exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- 2.52 parity: 
parity :: [Bool] -> Bool 
parity xs = let truths = filter id xs 
            in (length truths) `mod` 2 == 0 

--exercise 2.53
evenNr :: (a -> Bool) -> [a] -> Bool
evenNr p xs = parity (map p xs)