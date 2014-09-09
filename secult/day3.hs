module Count
  where


count1 p l = length (filter p l)

count2 p l = foldr (\x c -> if p x then c+1 else c) 0 l
{-
data Pair a b  :: Pair a b
pairFst (Pair x y) = x
pairSnd (Pair x y) = y
-}

firstElement :: [a] -> Maybe a
firstElement [] = Nothing
firstElement (x:xs) = Just x

findElement :: (a->Bool) -> [a] -> Maybe a
findElement p [] = Nothing
findElement p (x:xs) =
  if p x then Just x
  else findElement p xs

data Tuple a b c d= Tuple1 {a::Char}
                  | Tuple2 {a ::Char, b::b}
                  | Tuple3 {a::Char, b::b, c::c}
                  | Tuple4 {a::Char, b::b, c::c, d::d}
                  | SomKonstruktor {a ::Char}

tuple1 :: Tuple a b c d -> Maybe Char
tuple1 (Tuple1 a)  = Just a
tuple1 (Tuple2 a b)  = Just a
tuple1 (Tuple3 a b c)  = Just a
tuple1 (Tuple4 a b c d)  = Just a
tuple1 _= Nothing

tuple3 :: Tuple a b c d -> Maybe c
tuple3 (Tuple1 a)  = Nothing
tuple3 (Tuple2 a b)  = Nothing
tuple3 (Tuple3 a b c)  = Just c
tuple3 (Tuple4 a b c d)  = Just c
tuple3 (SomKonstruktor a)= Nothing

makeTuple :: Tuple a b c d -> Either Char (Either (Char,b) (Either (Char,b,c) (Char,b,c,d)))
makeTuple (Tuple1 a) = Left a
makeTuple (Tuple2 a b)  = Right (Left (a,b))
makeTuple (Tuple3 a b c)  = Right (Right (Left (a,b,c)))
makeTuple (Tuple4 a b c d)  = Right (Right (Right (a,b,c,d)))
makeTuple (SomKonstruktor a)= Left a
