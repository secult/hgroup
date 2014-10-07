module Lab5 where

import Data.List
import Week5
import Test.Hspec
--import Test.QuickCheck
import Data.Functor
--import ShowSudoku

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

--exercise 2, 1 hour

-- Still need to test this exercise

-- check if the solution is minimal and that none of its children are minimal
testMinimal :: Node -> Bool
testMinimal x = uniqueSol x && 
                foldr (\a b ->  (not b) && (not $ uniqueSol a) ) True (removeHints x)

-- get the states of the sudoku with one of the filled positions removed 
removeHints :: Node -> [Node]
removeHints (s,c) = getStates s (filledPositions s)
  
-- build up all sudoku states and find their constraints  
getStates :: Sudoku -> [(Row,Column)] -> [Node]
getStates s xs = map (\sud -> (sud,constraints sud)) sudokus
             where sudokus = foldr (\a b -> (eraseS s a) : b) [] xs        
             
             
--exercise 3 -- Jonatan version (inspired by juriaans version)
-- time spent: 30 minutes

genSudokuEmptyBlocks :: IO()
genSudokuEmptyBlocks = do
            [(s,c)] <- rsolveNs [emptyN]
            [list] <- getRandomItem meBeingLazyList
            let eSud = removeBlocks s list  
            z <- genProblem (eSud, constraints eSud)
            if length (snd z) > 27 then do
                showNode z
                putStrLn $ show $ testMinimal z
            else genSudokuEmptyBlocks
            
removeBlocks :: Sudoku -> [Int] -> Sudoku
removeBlocks s xs = foldr emptyBlock s xs
            
emptyBlock :: Int -> Sudoku -> Sudoku
emptyBlock  a s = let blockPositions = [[(x,y) | x <- z, y <- p] | z <- blocks, p <- blocks]
                  in foldl eraseS s (blockPositions !! a) 
            
-- Juriaans suggestion, and I approve, because lazy evaluation makes sure this list is never built up completely :)
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

--exercise 4 time spent: 1.5 hours (mostly analyzing week5 functions) 

{--
    Additional Formal specifications:
        Subgrids can overlap (the same rules apply other than that), 
        this means:
            let f be a function with:
                Domain: sudoku grid coordinates
                Codomain: sudoku subgrids + nrcSudoku subgrids
            
            for the nrcSudoku f is no longer injective as f may return 
            more than one subgrid for each coordinate.
            
            The consequence of this is that the constraint for the value 
            of a coordinate changes to:
                - the value must be unique in the row and column of the 
                  coordinate
                - the value must be unique in the Union of the 
                  coordinates of each subgrid the coordinate belongs to 
    
    Implementation:
        the only changing constraint is that subgrids can overlap 
        this change can be implemented as follows:
            - add nrcBlocks, which is the same as the blocks function, 
              but with the coordinates of the additional blocks in an 
              nrcSudoku
            - add function nrcbl, which is similar to bl, but uses 
              nrcBlocks
            - add function nrcSubGrid which is similar to subGrid, 
              but checks only the additional subgrids from the nrc sudoku
            - redefine the freeInSubgrid function to give back the 
              intersection of all free values in each subgrid of an 
              element
            - redefine consistent function to also check the nrcsudoku grid
            - redefine sameblock to also check nrcsudoku fields
              
        Additionally the following functionality has been added:
            - Added a control boolean nrcsudoku
                - If it is true, the program will use nrc sudokus
                - If it is false, the program will use regular sudokus
            - Added a nice way to show an nrc grid
            
    this has been implemented in the Week5 module provided as in this repo
    
    the solution of the nrc sudoku:
        +---------+-----------+---------+
        | 4   7 8 | 3   9   2 | 6 1   5 |
        |   +-----|---+   +---|-----+   |
        | 6 | 1 9 | 7 | 5 | 8 | 3 2 | 4 |
        | 2 | 3 5 | 4 | 1 | 6 | 9 7 | 8 |
        +---------+-----------+---------+
        | 7 | 2 6 | 8 | 3 | 5 | 1 4 | 9 |
        |   +-----|---+   +---|-----+   |
        | 8   9 1 | 6   2   4 | 7 5   3 |
        |   +-----|---+   +---|-----+   |
        | 3 | 5 4 | 9 | 7 | 1 | 2 8 | 6 |
        +---------+-----------+---------+
        | 5 | 6 7 | 2 | 8 | 9 | 4 3 | 1 |
        | 9 | 8 3 | 1 | 4 | 7 | 5 6 | 2 |
        |   +-----|---+   +---|-----+   |
        | 1   4 2 | 5   6   3 | 8 9   7 |
        +---------+-----------+---------+

--}

-- exercise 5

{-- 
    By implementing the new constraints in exercise 4 the way that is described 
    above, the program is actually already able to generate nrc sudokus.
    
    Testing:
      - check if problem x (generated from y) is consistent
      - check if problem x (generated from y) is minimal
      - check if the solution for problem x (generated from y) is equal to y 
--}

-- exercise 6

{--
    Method:
    
    Generate Sudoku x
    Generate Problem y from x
    - recursively apply simpleTechnique 1 and 2 until stuck 
    - try to solve rest of values by refution
    
--}

type Difficulty = Float

doHumanSolve :: IO Difficulty
doHumanSolve = do
                (solution, n) <- genRandomSudoku
                problem  <- genProblem (solution, n)
                return $ humanSolve solution problem

humanSolve :: Sudoku -> Node -> Difficulty
humanSolve sol prob | (sud2grid sol) == (sud2grid $ fst prob)    = 0
                    | (sud2grid $ fst prob) == (sud2grid $ fst logicSolve) = humanSolve sol logicSolve --Apply logic to solve sudoku
                    | otherwise          = undefined --Get mean refutation for open fields 
                    where
                       logicSolve = solveByLogic prob

solveByLogic :: Node -> Node
solveByLogic n = let emptyPos = [ (r,c) | r <- positions, c <- positions, (fst newN) (r,c) == 0 ]
                     newN = undefined --foldr with tech2 
                 in foldr tech1 newN emptyPos 

-- Single Position check if there is only one free position within (c,r,s)                
singlePosition :: (Row,Column) -> Node -> Node
singlePosition = undefined

-- Single Candidate
singleCandidate :: (Row,Column) -> Node -> Node
singleCandidate = undefined

-- for each empty position, check each value except for the actual value, try one of above techniques until contradiction 
refute :: Node -> Sudoku -> Difficulty
refute state solution = undefined




































getEmptyInColumn :: Sudoku -> [Column]
getEmptyInColumn s c = filter (\(r,c') -> c' == c ) (openPositions s)











