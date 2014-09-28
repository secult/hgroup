module Sol4 where


import SetOrd
import System.Random
import Test.QuickCheck


createSet :: Int -> RandomGen -> Set Int
createSet i gen = genAndIns emptySet i gen

genAndIns :: Set Int -> Int -> RandomGen -> Set Int
genAndIns set i gen | i < 1     = set
                    | otherwise = genAndIns (insertSet rndNum set) (i-1)
                        where rndNum = --next from gen!