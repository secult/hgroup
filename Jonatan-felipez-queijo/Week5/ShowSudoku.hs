module ShowSudoku where

import Data.List
import System.Random

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

type Node = (Sudoku,[Constraint])
type Sudoku = (Row,Column) -> Value

type Constraint = (Row,Column,[Value])


positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

---------------------------------------------------- Sudoku
showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

---------------------------------------------------- Regular grid
showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")
    
    
---------------------------------------------------- NRC grid
showNRCGrid :: Grid -> IO()
showNRCGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+-----------+---------+")
    showNRCRow as; 
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showSpecRow bs; showSpecRow cs
    putStrLn ("+---------+-----------+---------+")
    showSpecRow ds; 
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showNRCRow es; 
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showSpecRow fs
    putStrLn ("+---------+-----------+---------+")
    showSpecRow gs; showSpecRow hs; 
    putStrLn ("|   +-----|---+   +---|-----+   |")
    showNRCRow is
    putStrLn ("+---------+-----------+--------+")
   
showSpecRow :: [Value] -> IO()
showSpecRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
    mapM_ putStr 
    [ "| ", showVal a1, " | ",
            showVal a2, " ",
            showVal a3, " | ",
            showVal a4, " | ", 
            showVal a5, " | ",
            showVal a6, " | ",
            showVal a7, " ",
            showVal a8, " | ",
            showVal a9, " |" ++ ['\n']
    ]  
    

showNRCRow :: [Value] -> IO()
showNRCRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
    mapM_ putStr
    [ "| ", showVal a1, "   ",
            showVal a2, " ",
            showVal a3, " | ",
            showVal a4, "   ",
            showVal a5, "   ",
            showVal a6, " | ",
            showVal a7, " ",
            showVal a8, "   ",
            showVal a9, " |" ++ ['\n']      
    ]

---------------------------------------------------- Examples
example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]
            
exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]
    