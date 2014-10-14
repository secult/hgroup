import Test.Hspec
import Test.QuickCheck

exM :: Integer -> Integer -> Integer -> Integer
exM x 1 m = x `mod` m
exM _ 0 m = 1 `mod` m 
exM 0 _ m = 0---try changing the line order

exM x y m | (y `mod` 2) ==0 = (exM x (y `div` 2) m)^2 `mod` m
          | otherwise = (exM x (y-1) m) * x `mod` m --try changing the *

crappyHspecTest :: IO ()
crappyHspecTest = hspec $ do
	describe "Unit testing of validity" $ do
	    it "Tries a big number to modulate" $ do
	        let testNumber = exM 2097152 262144 314
	        let reference = ((2097152^262144) `mod` 314)
	        testNumber ==reference `shouldBe` (True::Bool)

crappyQuickCheckTest =
	quickCheck (\(Positive x) (Positive z) -> (exM x 0 z)==1 `mod` z)

compareResults :: Integer -> Integer-> Integer-> Bool
--invariant for quickCheck
compareResults a b c | c ==0 = True--only for quickcheck
compareResults a b c | b<0 = True--only for quickcheck
                    | otherwise = (exM (abs a) b (abs c)) == (abs a)^b `mod` (abs c)
                        