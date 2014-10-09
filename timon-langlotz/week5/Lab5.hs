module Lab5

where
import Data.List
import Data.Time
import Week5


-- test to prove Timon > Juriaan!
measure :: IO ()
measure = do 
        start <- getCurrentTime
        putStrLn $ show $ testMinimal minimal
        end <- getCurrentTime
        putStrLn $ show $ diffUTCTime end start
        
        let bla = initNode minimal
        start <- getCurrentTime
        putStrLn $ show $ testMinimal2 (bla)
        end <- getCurrentTime
        putStrLn $ show $ diffUTCTime end start


-- Exercise 2
minimal :: Grid
minimal = [[2,0,1,0,0,8,0,0,0],
           [0,0,0,7,0,0,0,0,0],
           [7,8,0,0,4,3,0,0,0],
           [0,0,9,8,0,0,0,6,0],
           [0,1,5,0,0,0,3,0,0],
           [0,0,0,0,0,7,0,5,0],
           [0,0,3,0,0,0,9,0,0],
           [5,0,0,0,0,9,0,0,6],
           [0,0,8,6,0,0,0,0,1]]
           
testMinimal :: Grid -> Bool
testMinimal grid = and (map uniqueSol (initNode grid)) && (not $ or (map uniqueSol (concat (map initNode grids))))
                   where grids = (updateAllValues grid (0,0))

updateAllValues :: Grid -> (Row,Column) -> [Grid]
updateAllValues grid (r,c) | r > 8                   = []
                           | c > 8                   = updateAllValues grid (r+1,0)
                           | ((grid !! r) !! c) == 0 = updateAllValues grid (r,c+1)
                           | otherwise               = sud2grid (eraseS (grid2sud grid) (r+1,c+1)) :
                                                       (updateAllValues grid (r,c+1))
                                                       
                                                       
-- Juriaans solution:
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

testMinimal2 :: [Node] -> Bool
testMinimal2 [] = False 
testMinimal2 [x] = length (solveNs [x]) == 1 && multipleSolutions (removeHints g) where
		g = sud2grid $ fst x


-- Exercise 3
create :: Int -> IO ()
create x = do 
            [r] <- rsolveNs [emptyN]
            list <- randomList [1..9] x
            let [sud] = initNode $ sud2grid (removeBlocks (fst r) list)
            z <- genProblem sud
            if testMinimal2 [z] then do 
                showNode z
                solveShowNs [z]
            else
                do
                    putStrLn "not minimal! trying again..."
                    create x

removeBlocks :: Sudoku -> [Int] -> Sudoku
removeBlocks sud [] = sud
removeBlocks sud xs = foldr (\i s -> removeBlock s i) sud xs
            
removeBlock :: Sudoku -> Int -> Sudoku
removeBlock s x = foldr (\pos sud -> eraseS sud pos) s (listPos x)

randomList :: [Int] -> Int -> IO [Int]
randomList xs 1 = getRandomItem xs
randomList xs x = do
                  e <- getRandomItem xs
                  let newList = filter (/= (head e)) xs
                  item <- randomList newList (x-1)
                  return (item ++ e)

-- translates a position of a block in the Sudoku (1..9) 
-- into the corresponding (Row,Column) pairs
listPos :: Int -> [(Row,Column)]
listPos x | x == 1 = [(x,y) | x <- [1..3],y <- [1..3]]
listPos x | x == 2 = [(x,y) | x <- [1..3],y <- [4..6]]
listPos x | x == 3 = [(x,y) | x <- [1..3],y <- [7..9]]
listPos x | x == 4 = [(x,y) | x <- [4..6],y <- [1..3]]
listPos x | x == 5 = [(x,y) | x <- [4..6],y <- [4..6]]
listPos x | x == 6 = [(x,y) | x <- [4..6],y <- [7..9]]
listPos x | x == 7 = [(x,y) | x <- [7..9],y <- [1..3]]
listPos x | x == 8 = [(x,y) | x <- [7..9],y <- [4..6]]
listPos x | x == 9 = [(x,y) | x <- [7..9],y <- [7..9]]


-- Exercise 4
-- check if subgrids at (2,2), (2,6), (6,2), (6,6) surjective
subgridSurjective :: Sudoku -> (Row,Column) -> Bool
subgridSurjective s (r,c) = surjective vs where 
   vs = filter (/= 0) (subGrid s (r,c))
   
surjective :: Eq a => [a] -> Bool
surjective xs = length (nub xs) <= length xs

-- Exercise 5
