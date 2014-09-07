--Juriaan Kennedy - 6061613
module Sol2 where

import GS
import TAMO

--exercise 2.13
check1a = not True <=> False 
check1b = not False <=> True
check2 = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
check3a = logEquiv1 (\ p -> p || True) (\ _ -> True)
check3b = logEquiv1 (\ p -> p && False) (\ _ -> False)
check4a = logEquiv1 (\ p -> p || False) id
check4b = logEquiv1 (\ p -> p && True) id
check5 = logEquiv1 (\ p -> p || not p) (\ _ -> True)
check6 = logEquiv1 (\p -> p && not p) (\ _ -> False)

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

--exercise 2.52
parity :: [Bool] -> Bool
parity xs = even (length xs)

--exercise 2.53
evenNr :: (a -> Bool) -> [a] -> Bool
evenNr p xs = parity (map p xs)