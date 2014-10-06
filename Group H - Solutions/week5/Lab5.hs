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

                     
-- Exercise 6
-- 1 -> easy, 10 -> hard
type Difficulty = Float

humanSolve :: Sudoku -> Node -> Difficulty
humanSolve sol prob | (sud2grid sol) == (sud2grid $ fst prob) = 1
                    | (sud2grid $ fst prob) /= (sud2grid $ fst logicSol) = humanSolve sol logicSol
                    | otherwise = refute sol prob
                        where logicSol = logicSolve prob
                    
                    
logicSolve :: Node -> Node
logicSolve (sud,con) = undefined --any map  (\ (sud',con')

nakedSingle :: [Constraint] -> Sudoku -> Sudoku
nakedSingle cs sud = foldr (\ (row,col,vs) sud' -> if (length vs == 1) 
                                                then extend sud ((row,col),head vs)
                                                else sud') sud cs
                                                
hiddenSingle :: Sudoku -> Sudoku
hiddenSingle sud = undefined --foldr (\ [row] sud 


hiddenSingleColumn :: Sudoku -> Sudoku
hiddenSingleColumn sud = foldr extSingleCol sud positions


extSingleCol :: Column -> Sudoku -> Sudoku
extSingleCol col sud | length value == 1 = extend sud (head (getEmptyInColumn sud col), head value)
                     | otherwise = sud
                            where value = freeInColumn sud col

getEmptyInColumn :: Sudoku -> Column -> [(Row,Column)]
getEmptyInColumn sud col = filter (\ (r,c) -> c == col) (openPositions sud)

hiddenSingleRow :: Sudoku -> Sudoku
hiddenSingleRow = undefined

getEmptyInRow :: Sudoku -> Row -> [Column]
getEmptyInRow sud row = undefined -- filter (\ (r,c) -> r == row) (openPositions sud)

hiddenSingleSubgrid :: Sudoku -> Sudoku
hiddenSingleSubgrid = undefined

getEmptyInSubgrid :: Sudoku -> (Row,Column) -> [Row]
getEmptyInSubgrid sud sg = undefined --filter (undefined) (openPositions sud)

eachSub :: [(Row,Column)]
eachSub = [(a,b) | a <- [2,5,8], b <- [2,5,8]]

refute :: Sudoku -> Node -> Difficulty
refute sud n = undefined

bruteSudoku :: Difficulty -> IO Sudoku
bruteSudoku = undefined

example1' :: Grid
example1' = [[5,3,0,0,7,0,0,0,0],
             [6,0,0,1,9,5,0,0,0],
             [1,9,8,0,0,0,0,6,0],
             [8,0,0,0,6,0,0,0,3],
             [4,0,0,8,0,3,0,0,1],
             [7,0,0,0,2,0,0,0,6],
             [9,6,0,0,0,0,2,8,0],
             [2,0,0,4,1,9,0,0,5],
             [0,0,0,0,8,0,0,7,9]]


