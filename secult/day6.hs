module Day6
 where
import Day5
--skoro_blowup =  foldl (\x y -> x++concatNTimes [y] (length x)) "a" "2345"
--hmm = foldl (\x y -> x:concatNTimes y (length m)) "a" m
  --    where m=["sa  asfa"]

--1.16

sortString :: String-> String
sortString []=[]
sortString [a]=[a]
sortString xs = let minOne=foldr1 (\x y-> if x<y then x else y) xs
            in minOne : (sortString (removeFst minOne xs))
