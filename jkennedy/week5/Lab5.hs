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

--exercise 4, 1 hour
--every subgrid [l,k] with l, k ranging over [2..4] and [6..8]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks 

nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
nrcSubGrid s (r,c) = 
  [ s (r',c') | r' <- nrcBl r, c' <- nrcBl c ]
  
freeInNrcSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInNrcSubgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))

nrcFreeAtPos :: Sudoku -> (Row,Column) -> [Value]
nrcFreeAtPos s (r,c) = freeAtPos s (r,c) `intersect` (freeInNrcSubgrid s (r,c))

nrcSubgridInjective :: Sudoku -> (Row,Column) -> Bool
nrcSubgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (nrcSubGrid s (r,c))
   
nrcConsistent :: Sudoku -> Bool
nrcConsistent s = consistent s && (and $ [ subgridInjective s (r,c) | r <- [2,6], c <- [2,6]])

extendNrcNode :: Node -> Constraint -> [Node]
extendNrcNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         nrcPrune (r,c,v) constraints) | v <- vs ]

nrcPrune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
nrcPrune _ [] = []
nrcPrune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | nrcSameblock (r,c) (x,y) = 
		(x,y,zs\\[v]) : nrcPrune (r,c,v) rest
  | otherwise = (x,y,zs) : nrcPrune (r,c,v) rest

nrcSameblock :: (Row,Column) -> (Row,Column) -> Bool
nrcSameblock (r,c) (x,y) = nrcBl r == nrcBl x && nrcBl c == nrcBl y 
		 
initNrcNode :: Grid -> [Node]
initNrcNode gr = let s = grid2sud gr in 
              if (not . nrcConsistent) s then [] 
              else [(s, nrcConstraints s)]
			  
nrcConstraints :: Sudoku -> [Constraint] 
nrcConstraints s = sortBy length3rd 
    [(r,c, nrcFreeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

solveNrcNs :: [Node] -> [Node]
solveNrcNs = search succNrcNode solved 

solveShowNrcNs :: [Node] -> IO()
solveShowNrcNs = sequence_ . fmap showNode . solveNrcNs

succNrcNode :: Node -> [Node]
succNrcNode (s,[]) = []
succNrcNode (s,p:ps) = extendNrcNode (s,ps) p 
					   
nrcExample1 :: Grid
nrcExample1 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

-- run solveShowNrcNs $ initNrcNode nrcExample1 for the solution to the NRC sudoku

--exercise 5, 30 min
emptyNrcN :: Node
emptyNrcN = (\ _ -> 0, nrcConstraints (\ _ -> 0))

rsuccNrcNode :: Node -> IO [Node]
rsuccNrcNode (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNrcNode (s,cs\\xs) (head xs))

rsolveNrcNs :: [Node] -> IO [Node]
rsolveNrcNs ns = rsearch rsuccNrcNode solved (return ns)

genRandomNrcSudoku :: IO Node
genRandomNrcSudoku = do [r] <- rsolveNrcNs [emptyNrcN]
                        return r

randomNrcS = genRandomNrcSudoku >>= showNode

uniqueNrcSol :: Node -> Bool
uniqueNrcSol node = singleton (solveNrcNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseNrcN :: Node -> (Row,Column) -> Node
eraseNrcN n (r,c) = (s, nrcConstraints s) 
  where s = eraseS (fst n) (r,c) 
  
minimalizeNrc :: Node -> [(Row,Column)] -> Node
minimalizeNrc n [] = n
minimalizeNrc n ((r,c):rcs) 
   | uniqueNrcSol n' = minimalizeNrc n' rcs
   | otherwise    = minimalizeNrc n  rcs
  where n' = eraseNrcN n (r,c)

genNrcProblem :: Node -> IO Node
genNrcProblem n = do ys <- randomize xs
                     return (minimalizeNrc n ys)
   where xs = filledPositions (fst n)

genSolveNrcSud :: IO ()
genSolveNrcSud = do [r] <- rsolveNrcNs [emptyNrcN]
                    showNode r
                    s  <- genNrcProblem r
                    showNode s