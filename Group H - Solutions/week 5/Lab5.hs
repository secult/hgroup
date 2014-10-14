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

--exercise 2, 1 hour
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

--exercise 3, 3 hours
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
            
-- lazy evaluation makes sure this list is never built up completely :)
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
	 
-- Exercise 6
{-- 
    This sudoku solver was based on the simple solver in the Pelánek paper mentioned in the exercise. 
    Although it is simplified due to shortage of time.
    
    it works like this:
    1 given a solution x, generate problem y from it
    2 try to solve problem y using two simple rules:
        - Naked value
        - Hidden single
    3 solved? great, the difficulty is 0
    4 stuck? get a hint, increase difficulty in step 3 by 1 and repeat from step 2
    
    
--}

type Difficulty = Int

doHumanSolve :: IO Difficulty
doHumanSolve = do 
                let sol = fst $ head $ solveNs $ initNode example2
                let prob = head $ initNode example2
                return $ humanSolve sol prob

humanSolve :: Sudoku -> Node -> Difficulty
humanSolve sol prob | (sud2grid sol) == (sud2grid $ fst prob) = 0
                    | (sud2grid $ fst prob) /= (sud2grid $ fst logicSol) = humanSolve sol logicSol
                    | otherwise = 1 + humanSolve sol (addEasiestCoordinate sol prob)
                        where logicSol = logicSolve prob
                        
addEasiestCoordinate :: Sudoku -> Node -> Node
addEasiestCoordinate sol (sud,con) = (easier, constraints easier)
                                where
                                    easier = extend sud ((x,y), sol (x,y)) 
                                    (x,y,_) = head con
                    
logicSolve :: Node -> Node
logicSolve (sud,con) = (sud', constraints sud) 
                     where sud' = hiddenSingle $ (nakedSingle con sud) 

-- Naked Single -----------------------------------------------------  
nakedSingle :: [Constraint] -> Sudoku -> Sudoku
nakedSingle cs sud = foldr (\ (row,col,vs) sud' -> if (length vs == 1) 
                                                then extend sud ((row,col),head vs)
                                                else sud') sud cs
                                                
---------------------------------------------------------------------

-- Hidden Single ----------------------------------------------------                                               
hiddenSingle :: Sudoku -> Sudoku
hiddenSingle = hiddenSingleColumn.hiddenSingleRow.hiddenSingleSubgrid 

-- Column
hiddenSingleColumn :: Sudoku -> Sudoku
hiddenSingleColumn sud = foldr extSingleCol sud positions

extSingleCol :: Column -> Sudoku -> Sudoku
extSingleCol col sud | length value == 1 = extend sud (head (getEmptyInColumn sud col), head value)
                     | otherwise = sud
                            where value = freeInColumn sud col

getEmptyInColumn :: Sudoku -> Column -> [(Row,Column)]
getEmptyInColumn sud col = filter (\ (r,c) -> c == col) (openPositions sud)

-- Row
hiddenSingleRow :: Sudoku -> Sudoku
hiddenSingleRow sud = foldr extSingleRow sud positions

extSingleRow :: Row -> Sudoku -> Sudoku
extSingleRow row sud | length value == 1 = extend sud (head (getEmptyInRow sud row), head value)
                     | otherwise = sud
                            where value = freeInRow sud row
                            
getEmptyInRow :: Sudoku -> Row -> [(Row,Column)]
getEmptyInRow sud row = filter (\ (r,c) -> r == row) (openPositions sud)

-- SubGrid
hiddenSingleSubgrid :: Sudoku -> Sudoku
hiddenSingleSubgrid sud = foldr extSingleSubgrid sud eachSub

extSingleSubgrid :: (Row,Column) -> Sudoku -> Sudoku 
extSingleSubgrid sg sud | length value == 1 = extend sud (head (getEmptyInSubgrid sud sg), head value)
                        | otherwise = sud
                            where value = freeInSubgrid sud sg

getEmptyInSubgrid :: Sudoku -> (Row,Column) -> [(Row,Column)]
getEmptyInSubgrid sud sg = filter (\coord -> sud coord == 0) (subGridCoords sg)

eachSub :: [(Row,Column)]
eachSub = [(a,b) | a <- [2,5,8], b <- [2,5,8]]

subGridCoords :: (Row,Column) -> [(Row,Column)]
subGridCoords (r,c) = [ (r',c') | r' <- bl r, c' <- bl c]

------------------------------------------------------------------------------

generateEasySudoku :: IO ()
generateEasySudoku = do
                      x <- (bruteSudoku (< 35))
                      showSudoku x

generateHardSudoku :: IO ()
generateHardSudoku =  do
                      x <- (bruteSudoku (> 38))
                      showSudoku x

bruteSudoku :: (Difficulty -> Bool) -> IO Sudoku
bruteSudoku diff = do 
                    sol  <- genRandomSudoku
                    prob <- genProblem sol
                    -- debug: showNode sol
                    let difficulty = humanSolve (fst sol) prob
                    -- debug: putStrLn $ show difficulty
                    if diff difficulty 
                    then return $ fst prob
                    else bruteSudoku diff

             
             
