module Day5
  where

--1.9

maxInt :: [Int]->Int
maxInt [] = error "none"
maxInt [a] = a
maxInt (x:xs) = max x $maxInt xs

--1.10
removeFst ::Eq a=> a -> [a]-> [a]
removeFst m [] =[]
removeFst m [a] | m ==a =[]
removeFst m (x:xs) | m ==x =xs
        | otherwise = x:(removeFst m xs)

--1.11
sortInts :: [Int]-> [Int]
sortInts []=[]
sortInts [a]=[a]
sortInts xs = minimum xs : (sortInts (removeFst (minimum xs) xs))
--or as i can see
sortInts xs = (m : (sortInts (removeFst m xs)) )where
              m=(minimum xs)

--1.12
average :: [Int] -> Rational
average [] = error "empty"
average xs = toRational (sum xs) /toRational (length xs)
---1.13
count :: Char -> String -> Int
count _ [] = 0
count a xs = length $filter (==a) xs
--1.14
concatNTimes ::String ->  Int -> String
concatNTimes a 0 = a
concatNTimes a n = a ++ (concatNTimes a (n-1))

blowup :: String -> String
blowup [] = []
