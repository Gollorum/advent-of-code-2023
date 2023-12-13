module D13 (execSample, execFinal) where

import Data.List (find, transpose)
import Data.List.Split (splitOn)

execSample :: IO ()
execSample = exec "input/sample_13.txt"

execFinal :: IO ()
execFinal = exec "input/13.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = splitOn [""] (lines content) :: [[String]]
  let horizontals = sum (map (horizontalReflectionAt 1) ls)
  let verticals = sum (map (verticalReflectionAt 1) ls)
  print (verticals + 100 * horizontals)

--reflectionsAt :: [String] -> (Int, Int)
--reflectionsAt rows = (horizontalReflectionAt rows, horizontalReflectionAt (transpose rows))

horizontalReflectionAt :: Int -> [String] -> Int
horizontalReflectionAt smudges rows = case find (\i -> smudges == diff (reverse (take i rows)) (drop i rows)) [1..(length rows - 1)] of
  Just row -> row
  Nothing -> 0

verticalReflectionAt :: Int -> [String] -> Int
verticalReflectionAt smudges rows = horizontalReflectionAt smudges (transpose rows)

diff :: [String] -> [String] -> Int
diff a b = sum (zipWith lineDiff a b)

lineDiff :: String -> String -> Int
lineDiff a b = length [i | (i, j) <- a `zip` b, i /= j]