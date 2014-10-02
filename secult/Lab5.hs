module Lab5
  where
import Data.List
import Week5
import Test.Hspec
import System.Random
import Control.Monad
-- exercise 1:
-- we need to specify preconditions and postconditions from hoare triple
hsptest :: IO ()
hsptest = hspec $ do
  describe "Testing pre and postconditions of the sudoku solver" $ do
  describe "Preconditions" $ do
    it "every row should have 9 items" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs

      (all ((==9).length) a) `shouldBe` True
    it "there should be 9 rows" $ do
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
  describe "Postconditions" $ do
    it "every row should have 9 items" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs

      (all ((==9).length) a) `shouldBe` True
    it "there should be 9 rows" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs

      (length a) `shouldBe` 9
    it "every row should contain numbers [1..9] only once" $ do --just nub
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs
      (all ((\l -> length l == (length . nub $ l)).(\k->filter (/=0) k)) a) `shouldBe` True
    it "every row should contain numbers only in 1~9 range" $ do
      grs<- genRandomSudoku
      let a=sud2grid . fst $grs
      (all (==True) [all (`elem`[1..9]) y|y<-a]) `shouldBe` True

  it "every column should contain each number {1..9}" $ do
    grs<- genRandomSudoku
    let a=sud2grid . fst $grs
    () `shouldBe` ()
    -- ...
  it "every subgrid should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every row should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every column should contain each number {1..9}" $ do
    () `shouldBe` ()
  it "every subgrid should contain each number {1..9}" $ do
    () `shouldBe` ()

-- exercise 2:
--time spent 6h
--but this is actually not minimal atm

_eraseRandomOne :: Node -> StdGen -> (Node, StdGen)
_eraseRandomOne a rndGen =  (eraseN a (1+(x `mod` 9),1+(y `mod` 9)),h) where
  (x,g)=next rndGen
  (y,h)=next g


eraseUntilMinimal:: Node -> Node


eraseUntilMinimal s | uniqueSol s = s

fuc :: (Node,StdGen)-> Node
fuc (rs,stdGen) | (\(rs,gen) -> not (uniqueSol rs)) (_eraseRandomOne rs stdGen) = rs
              |otherwise = fuc (_eraseRandomOne rs stdGen)

doErase :: IO Node
doErase = do
  rs<-genRandomSudoku
  stdGen<-getStdGen
  let g=fuc (rs,stdGen)
  showNode g
  return g
