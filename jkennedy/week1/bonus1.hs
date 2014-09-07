module Lab1Bonus

where

--exercise 1
length' :: [a] -> Int
length' xs = foldr (\_ -> (+1)) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldr (\ y acc -> if y == x then True else acc) False xs

--or takes a list of booleans and and if any of the booleans in the list are True, it returns True.
or' :: [Bool] -> Bool
or' = foldr (||) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\ x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\ x acc -> if p x then x : acc else acc) [] xs

foldrplusplus :: [a] -> [a] -> [a]
foldrplusplus xs ys = foldr (\ x acc -> x : acc) ys xs

--could not find reversal as a Haskell function, I assume the intended function is reverse
reverse' :: [a] -> [a]
reverse' xs = foldr (\ x acc -> acc ++ [x]) [] xs

--exercise 2
reverseLeft :: [a] -> [a]
reverseLeft xs = foldl (\ acc x -> x : acc) [] xs