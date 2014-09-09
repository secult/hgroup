module Sol3

where

import Lab1Bonus

-- Exercise 1
length' :: [a] -> Int
length' as         = foldr (\_ n -> n + 1) 0 as

elem' :: Eq a          => a -> [a] -> Bool
elem' a as         = foldr (\b c -> c || (b == a)) False as

or' :: [Bool] -> Bool
or' bs             = foldr (||) False bs

map' :: (a -> b) -> [a] -> [b]
map' f as          = foldr (\b c -> (f b):c) [] as

filter' :: (a -> Bool) -> [a] -> [a]
filter' f as = foldr (\b c -> if f b then b:c else c) [] as


--Exercise 2
reverse' :: [a] -> [a]
reverse' as        = foldl (\b c -> c:b) [] as

-- Exercise 5
john2, bill2 :: (Islander,Islander) -> Bool
john2(x,y) = x == y 
bill2 (x,y) = x /= y

solution5 :: [(Islander,Islander)]
solution5 = [(x,y) | x <- [Knight,Knave],
			y <- [Knight,Knave],
			(john2 (x,y) == (x == Knight)) && (bill2 (x,y) == (y == Knight))]

-- Exercise 7
solution :: [Boy]
solution = 

honest :: [Boy]
