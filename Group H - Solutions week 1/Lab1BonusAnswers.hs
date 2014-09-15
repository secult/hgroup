module Lab1BonusExercises where

import Lab1Bonus

-- Exercise 1.1 length in terms of foldr
length' :: [a] -> Int
length' = foldr (\_ -> (+1)) 0

-- Exercise 1.2 elem x in terms of foldr
elem' :: Eq a => a -> [a] -> Bool
elem' x xs = foldr (\a b -> b || a == x) False xs
-- checking b first is more efficient, since if b is True, 
-- then you have already seen the element you want

-- Exercise 1.3 or in terms of foldr
or' :: [Bool] -> Bool
or' = foldr (||) False

-- Exercise 1.4 map in terms of foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\a b -> f a : b) [] xs

-- Exercise 1.5 filter p in terms of foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\a b -> if f a then a:b else b) [] xs

-- Exercise 1.6 (++) in terms of foldr
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

-- Exercise 1.7 reversal in terms of foldr
reversal :: [a] -> [a]
reversal = foldr (\a b -> b ++ [a]) []

-- Exercise 2
reversal' :: [a] -> [a]
reversal' = foldl (flip (:)) [] 

-- Exercise 3 due to laziness, foldr will sometimes work with an infinite list 
-- (if the function you use to fold is not strict).

-- Exercise 4 Implement solution2 (the lady or the tiger?)
sign3, sign4 :: (Creature, Creature) -> Bool
sign3 (x,y) = x == Lady || y == Lady
sign4 (x,y) = x == Tiger 

solution2 :: [(Creature, Creature)]
solution2 =
    [(x,y) | x <- [Lady, Tiger]
            ,y <- [Lady, Tiger]
            ,sign3 (x,y) == sign4 (x,y)]

-- Exercise 5 Implement solution3 (Knights and Knaves)
john' :: (Islander, Islander) -> Bool
john' (x,y) = x == y

bill' :: (Islander, Islander) -> Bool
bill' (x,y) = x /= y

solution4 :: [(Islander, Islander)]
solution4 = [(x,y) | x <- [Knight, Knave]
                    ,y <- [Knight, Knave]
                    ,(bill' (x,y) == (y == Knight)) && (john'(x,y) == (x == Knight))]
                    -- bill's can only be true if he himself is a knight, the same for john
                     
-- Exercise 7 Implement solution (INCOMPLETE)

-- get all valid permutations of Honest/Liar values (3 liars, 2 honest)
boolPerms = let valid xs = length (filter id xs) == 2
            in filter valid  [[a,b,c,d,e] | a <- [True, False]
                                           ,b <- [True, False]
                                           ,c <- [True, False]
                                           ,d <- [True, False]
                                           ,e <- [True, False]]

-- Reverse a statement if the boy is lying (need to find a situation where exactly the neg. of 2 statements is True 
-- and the other three statements are also True
reverseIfLie :: Bool -> Bool -> Bool
reverseIfLie lie x = if not lie then x else not x 


