module Lab5

where
import Data.List
import Week5
import Test.Hspec
import Test.QuickCheck
import Data.Functor

--exercise 1, 3 hours + jonatans research, which returned nothing, thanks...
sudokuSpecs :: IO()
sudokuSpecs = hspec $ do
	describe "complete sudoku" $ do
		describe "every row" $ do
			it "has each number from 1..9" $ do
				g <- sud2grid.fst <$> genRandomSudoku
				and (map hasAllNumbers g) `shouldBe` True
		describe "every column" $ do
			it "has each number from 1..9" $ do
				s <- fst <$> genRandomSudoku
				and (map hasAllNumbers [[s (i,c) | i <- positions] | c <- positions]) `shouldBe` True
		describe "every subgrid" $ do
			it "has each number from 1..9" $ do
				s <- fst <$> genRandomSudoku
				and (map hasAllNumbers [subGrid s (r,c) | r <- [1,4,7], c <- [1,4,7]]) `shouldBe` True
	describe "partial sudoku" $ do
		describe "every row" $ do
			it "has unique values and the values are in 1..9" $ do
				solved <- genRandomSudoku
				g <- sud2grid.fst <$> genProblem solved
				and (map inRangeAndNoDups g) `shouldBe` True
		describe "every column" $ do
			it "has unique values and the values are in 1..9" $ do
				solved <- genRandomSudoku
				s <- fst <$> genProblem solved
				and (map inRangeAndNoDups [[s (i,c) | i <- positions] | c <- positions]) `shouldBe` True
		describe "every subgrid" $ do
			it "has unique values and the values are in 1..9" $ do
				solved <- genRandomSudoku
				s <- fst <$> genProblem solved
				and (map inRangeAndNoDups [subGrid s (r,c) | r <- [1,4,7], c <- [1,4,7]]) `shouldBe` True



hasAllNumbers :: [Value] -> Bool
hasAllNumbers x = nub x == x && sum x == sum [1..9] && not (any (<= 0) x)

inRangeAndNoDups :: [Value] -> Bool
inRangeAndNoDups a = nub x == x && x \\ [1..9] == []
					 where x = filter (/=0) a