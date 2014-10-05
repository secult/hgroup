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
listPos x | x == 1 = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
listPos x | x == 2 = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
listPos x | x == 3 = [(1,7),(1,8),(1,9),(2,7),(2,8),(2,9),(3,7),(3,8),(3,9)]
listPos x | x == 4 = [(4,1),(4,2),(4,3),(5,1),(5,2),(5,3),(6,1),(6,2),(6,3)]
listPos x | x == 5 = [(4,4),(4,5),(4,6),(5,4),(5,5),(5,6),(6,4),(6,5),(6,6)]
listPos x | x == 6 = [(4,7),(4,8),(4,9),(5,7),(5,8),(5,9),(6,7),(6,8),(6,9)]
listPos x | x == 7 = [(7,1),(7,2),(7,3),(8,1),(8,2),(8,3),(9,1),(9,2),(9,3)]
listPos x | x == 8 = [(7,4),(7,5),(7,6),(8,4),(8,5),(8,6),(9,4),(9,5),(9,6)]
listPos x | x == 9 = [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]


-- Exercise 4
-- check if subgrids at (2,2), (2,6), (6,2), (6,6) surjective
subgridSurjective :: Sudoku -> (Row,Column) -> Bool
subgridSurjective s (r,c) = surjective vs where 
   vs = filter (/= 0) (subGrid s (r,c))
   
surjective :: Eq a => [a] -> Bool
surjective xs = length (nub xs) <= length xs

-- Exercise 5
