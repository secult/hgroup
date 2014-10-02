module Lab5

where
import Data.List
import Week5
import Test.Hspec
import Test.QuickCheck
import Data.Functor

--exercise 1, 4 hours 
{--

Can you use QuickCheck?
Yes you could use quickcheck, but you would have to do some hacking:
- You would have to wrap the 'Grid' type synonym into a datatype with it's own constructor, 
  because instances do not work with Type synonyms.
- After this, you would have to make this datatype an instance of Arbitrary. 
    - Due to the constraints of a Grid (length, width, consistency, ...) you should provide your own Gen Grid
    - You can not do this with any of the functions provided in the week5 module, 
      because almost all of these use IO and this is not allowed in Arbitrary. 

So this basically means that using quickcheck would require you to either make your own sudoku generator and make that a Gen Grid
or replacing all of the IO monads in Week5 with Gen monads.

Since we can already generate random Sudoku problems and test this function, we do not use QuickCheck just for the sake of using QuickCheck.

--}

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
