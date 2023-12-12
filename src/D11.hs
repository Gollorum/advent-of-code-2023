module D11 (execSample, execFinal) where

import Data.List (transpose)

execSample :: IO ()
execSample = exec "input/sample_11.txt"

execFinal :: IO ()
execFinal = exec "input/11.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let grid = lines content
  let rowEx = expandedRows grid
  let colEx = expandedColumns grid
  print ((sum . distances rowEx colEx 999999 . allGalaxies) grid)
  
expandedRows :: [[Char]] -> [Int]
expandedRows rows = [y | (y, row) <- zip [0..] rows, all (=='.') row]
  
expandedColumns :: [[Char]] -> [Int]
expandedColumns = expandedRows . transpose

allGalaxies :: [[Char]] -> [(Int, Int)]
allGalaxies grid = [(x, y) | (x, y, char) <- concatMap (\(y, row) -> map (\(x, char) -> (x, y, char)) (zip [0..] row)) ([0..] `zip` grid), char == '#']

distances :: [Int] -> [Int] -> Int -> [(Int, Int)] -> [Int]
distances rowEx colEx expansionRate galaxies = concatMap (\i -> map (\j -> distance rowEx colEx expansionRate (galaxies!!i) (galaxies!!j)) [(i+1) .. (length galaxies - 1)]) [0..(length galaxies - 1)]

distance :: [Int] -> [Int] -> Int -> (Int, Int) -> (Int, Int) -> Int
distance rowEx colEx expansionRate (xa, ya) (xb, yb) = abs (xb - xa) + abs (yb - ya) + expansionRate * (length (filter (`elem` colEx) [(min xa xb)..(max xa xb)]) + length (filter (`elem` rowEx) [ya..yb]))