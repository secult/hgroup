module GS

where

divides :: Integer -> Integer -> Bool
divides d n= rem n d == 0

ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer

ldf k n | divides k n =k
ldf k n | k^2>n = n
ldf k n = ldf (k+1) n

prime0 n | n<1 = error "not a pos int"
  | n == 1 = False
  | otherwise = ld n == n

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)



maxInt ::[Int]-> Int
maxInt [] = error "end of list"
maxInt [x] = x
maxInt (x:xs) | x >= (maximum xs) = x
maxInt (x:xs) = maxInt xs

removeFst :: [Integer] -> Integer ->[Integer]
removeFst  [] _ = []
removeFst (x:xs) m | x==m = xs
removeFst (x:xs) m = m:removeFst xs m

{-actually a sum of list
whereThing ::[Integer] -> Integer
whereThing [] = 0
whereThing [a] = a
whereThing (x:xs) = x+ whereThing xs

srtInts :: [Int]-> [Int]
srtInts []=[]
srt xs = m : (srtInts(removeFst m xs)) where m = mnmInt xs
-}

square ::Num x => x-> x

square x= x^2
