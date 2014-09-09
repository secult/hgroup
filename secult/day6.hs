module Day6
 where
import Day5
--skoro_blowup =  foldl (\x y -> x++concatNTimes [y] (length x)) "a" "2345"
--hmm = foldl (\x y -> x:concatNTimes y (length m)) "a" m
  --    where m=["sa  asfa"]

--1.15
--actually, this is not in the exercise
badSortString :: String-> String
badSortString []=[]
badSortString [a]=[a]
badSortString xs = let minOne=foldr1 (\x y-> if x<y then x else y) xs
            in minOne : (badSortString (removeFst minOne xs))

--but it does function actually
sortString :: [String]-> [String]
sortString []=[]
sortString [a]=[a]
sortString xs = let minOne=foldr1 (\x y-> if x<y then x else y) xs
            in minOne : (sortString (removeFst minOne xs))

--1.16
