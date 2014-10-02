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

--exercise 2, 2 hours
--not very efficient, replace nub with checking for a zero when removing a value
removeHints :: Grid -> [Grid]
removeHints g = nub $ map (removeValues g) rowsAndColumns

removeValues:: Grid -> (Row, Column) -> Grid
removeValues g rc = sud2grid (extend (grid2sud g) (rc, 0))

rowsAndColumns :: [(Int,Int)]
rowsAndColumns = [(x,y) | x <- positions, y <- positions]

multipleSolutions :: [Grid] -> Bool
multipleSolutions [] = False
multipleSolutions (x:xs) | length (solveNs (initNode x)) > 1 = True
			 | otherwise = multipleSolutions xs

testMinimal :: [Node] -> Bool
testMinimal [] = False 
testMinimal [x] = length (solveNs [x]) == 1 && multipleSolutions (removeHints g) where
		g = sud2grid $ fst x

--exercise 3, 3 hours
genSudokuEmptyBlocks :: IO()
genSudokuEmptyBlocks = do 
			[r] <- rsolveNs [emptyN]
			[list] <- getRandomItem meBeingLazyList
			let [x] = initNode $ sud2grid (removeBlocks (fst r) list)
			z <- genProblem x
			if length (snd z) > 27 then do
				showNode z
				putStrLn $ show $ testMinimal [z]
			else genSudokuEmptyBlocks

removeBlocks :: Sudoku -> [Int] -> Sudoku
removeBlocks s [x] = emptyBlock s x
removeBlocks s (x:xs) = emptyBlock (removeBlocks s xs) x

emptyBlock :: Sudoku -> Int -> Sudoku
emptyBlock s a = foldr (\x acc -> eraseS acc x) s singleBlock where
		singleBlock = blockPositions !! a

blockPositions :: [[(Row, Column)]]
blockPositions = [[(x,y) | x <- a, y <- b] | a <- blocks, b <- blocks]

--I dont want to bother with creating a random function that gives me a list of 3 unique integers
-- so I create a list with all posibilities and use the getRandomItem function from Week5
meBeingLazyList :: [[Int]]
meBeingLazyList = [[x,y,z] | x <- [0..8], y <- [0..8], z <- [0..8], x /= y, y /= z, x /= z]

{-The only problems we found with 4 or more empty blocks, were problems that looked like the following:
+-------+-------+-------+
| 7 2 6 | 3 8 4 |       |
| 9 3 5 | 7 6 1 |       |
| 8 4 1 | 5 9 2 |       |
+-------+-------+-------+
| 4 8 3 | 2 5 6 | 1 9 7 |
| 5 7 9 | 1 4 3 | 2 6 8 |
| 6 1 2 | 8 7 9 | 3 4 5 |
+-------+-------+-------+
|       |       |       |
|       |       |       |
|       |       |       |
+-------+-------+-------+
There could still be a possibility that a proper Sudoku with 4 empty blocks exists, however because we generate our problems randomly, finding this problem can take infinitely long. We were not prepared to wait that long, so we googled the answer: http://puzzling.stackexchange.com/questions/309/what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have
-}

--exercise 4

