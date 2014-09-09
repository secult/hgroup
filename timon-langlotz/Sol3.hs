module Sol3

where

-- Exercise 1
length' :: [a] -> Int
length' as         = foldr (\_ n -> n + 1) 0 as

elem' :: Eq a          => a -> [a] -> Bool
elem' a as         = foldr (\b c -> c || (b==a)) False as

or' :: [Bool] -> Bool
or' bs             = foldr (||) False bs

map' :: (a -> b) -> [a] -> [b]
map' f as          = foldr (\b c -> (f b):c) [] as

filter' :: (a -> Bool) -> [a] -> [a]
filter' f as = foldr (\b c -> if f b then b:c else []:c) [] as


--Exercise 2
reverse' :: [a] -> [a]
reverse' as        = foldl (\b c -> c:b) [] as
