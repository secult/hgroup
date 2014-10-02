module Week5 where 

import Data.List
import System.Random
import ShowSudoku

-- get the block coordinates that the coordinate belongs to Note NRC can have multiple, so redefine this
bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

-- get the subgrid corresponding to the (R,C) Note: NRC can have multiple, so redefine this
subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

-- check what values are not used in a sequence (used for Subgrid) 
freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

-- what values are not used in a row
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

-- what values are not used in the column  
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

-- what values are not used in a subgrid Note: for NRC subgrids can overlap, check
freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

-- check what values are available for this position
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = (freeInRow    s r) `intersect` 
                    (freeInColumn s c) `intersect` 
                    (freeInSubgrid s (r,c)) 

-- check if list has duplicates                    
injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

-- check if row has duplicates (except 0)
rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

-- check if col has duplicates (except 0)   
colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

-- check if subgrid has duplicates (except 0)
subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

-- check if Sudoku is consistent
consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]

-- update a sudoku
extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

-- show a node 
showNode :: Node -> IO()
showNode = showSudoku . fst

-- extend a node
extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) =           -- Extend the node by:
   [(extend s ((r,c),v),                        -- Update the sudoku in the node with value v
     sortBy length3rd $                         -- Update the sudoku constraints (sort ant prune)
         prune (r,c,v) constraints) | v <- vs ] -- for each v in vs (constraint values)

-- Constraint ordering: order on length of list with possible values 
length3rd :: Constraint -> Constraint -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')

-- after updating a value, remove it from the list of constraints in that row, column and block Note: of interest for NRC
prune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x                = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y                = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise             = (x,y,zs)      : prune (r,c,v) rest

  
-- check if the Row and columns are in the same block NB: NRC  
sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 


-- Turn a grid into a node if it is consistent
initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

-- return all the open positions of a sudoku
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

-- Get the sorted constraints of a sudoku                            
constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | (r,c) <- openPositions s ]

    
------- Tree stuff

-- make the tree grow    
grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

--count Tree nodes
count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

-- Search the tree and return all children that satisfy the goal
search :: (node -> [node]) -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal 
                                ((children x) ++ xs)

 
-- search  
solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

-- check if Node is solved (no available values left)
solved  :: Node -> Bool
solved = null . snd

-- build the children of the node
succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

-- solve the Grid and show it 
solveAndShow :: Grid -> IO()
solveAndShow gr = solveShowNs (initNode gr)

-- solve and show nodes
solveShowNs :: [Node] -> IO()
solveShowNs = sequence_ . fmap showNode . solveNs

-- the empty sudoku with empty constraints
emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

-- get a positive integer bounded by the parameter
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- get a random item of the list
getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt (length xs - 1)
                      return [xs !! n]

-- randomize a list 
randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y then return []
                  else 
                    do ys <- randomize (xs\\y)
                       return (head y:ys)

-- check if the constraint have an equal length                            
sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

-- return a random constraint
getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

-- get a list of successornodes with random constraints        
rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode (s,cs\\xs) (head xs))

-- reverseSolve a sudoku (make a minimal sudoku out of a complete one)
rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

-- ?
rsearch :: (node -> IO [node]) -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs then return []
     else 
       if goal (head xs) then return [head xs]
       else 
        do ys <- rsearch succ goal (succ (head xs))
           if (not . null) ys then return [head ys]
           else 
             if null (tail xs) then return []
             else rsearch succ goal (return $ tail xs)

-- generate a random sudoku
genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

-- show random sudoku
randomS = genRandomSudoku >>= showNode

-- check if a node has a unique solution
uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) 
  where 
    singleton [] = False
    singleton [x] = True
    singleton (x:y:zs) = False

-- set the value of a sudoku field to 0
eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) = update s ((r,c),0)

-- set the value of a node field to 0
eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

-- try to erase each hint to get a minimalized sudoku
minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
                        where n' = eraseN n (r,c)

-- get all the filledPositions
filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = 
  [ (r,c) | r <- positions,  
            c <- positions, s (r,c) /= 0 ]

-- generate a sudoku problem out of sudoku solution
genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

