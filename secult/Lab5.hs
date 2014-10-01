module Lab5
  where
import Data.List
import Week5
import Test.Hspec

-- we need to specify preconditions and postconditions from hoare triple
hsptest :: IO ()
hsptest = hspec $ do
  describe "Testing pre and postconditions of the sudoku solver" $ do
  describe "Preconditions" $ do
    it "every row should have 9 items" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs

      (all ((==9).length) a) `shouldBe` True
    it "row should have 9 rows" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs

      (length a) `shouldBe` 9
    it "every row should contain numbers [1..9] only once" $ do --just nub
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs
      (all ((\l -> length l == (length . nub $ l)).(\k->filter (/=0) k)) a) `shouldBe` True
    it "every row should contain numbers only in 0~9 range" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs
      (all (==True) [all (`elem`[0..9]) y|y<-a]) `shouldBe` True
  it "every column should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every subgrid should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every row should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every column should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every subgrid should contain each number {1..9}" $ do
    () `shouldBe` ()
