module Tutorial_hspec where

import Test.Hspec
import Data.List

main :: IO ()
main = hspec $ do 
    describe "toNegative" $ do
        it "returns the negative value of a positive number" $
            toNegative (-3) (-3) (*) `shouldBe` (-9)
        it "return a negative number without changing it" $
            toNegative (-5) 10 (+) `shouldBe` (-5)
        it "return zero if zero" $
            toNegative 0 0 (*) `shouldBe` 0
            
            
    describe "giveAppropriateAnswer" $ do
        it "returns an appropriate answer if you like Haskell" $
            giveAppropriateAnswer "I love Haskell" `shouldBe` "You stupid or what?"
        it "and also if you don't!" $
            giveAppropriateAnswer "I hate Haskell" `shouldBe` "You right dude. Haskell sucks!"
            
    describe "isInfixOf" $ do
        it "returns True if the first list is contained in the second" $
            isInfixOf "abc" "000abc000" `shouldBe` True
        it "returns False otherwise" $
            isInfixOf "abc" "aabbcc" `shouldBe` False

toNegative :: Int -> Int -> (Int -> Int -> Int) -> Int
toNegative x y f | (f x y) < 0 = f x y
                 | otherwise = (-1) * (f x y)
                 
giveAppropriateAnswer :: String -> String
giveAppropriateAnswer s | (isInfixOf "love" s || isInfixOf "like" s) && 
                          (isInfixOf "haskell" s || isInfixOf "Haskell" s) = "You stupid or what?"
                        | otherwise = "You right dude. Haskell sucks!"
                 