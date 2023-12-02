module D02 (execSample, execFinal) where

import Text.Regex.TDFA
import Data.List.Split (splitOn)

execSample :: IO ()
execSample = exec "input/sample_02.txt"

execFinal :: IO ()
execFinal = exec "input/02.txt"

numFrom :: String -> String -> Integer
numFrom t r = case (t =~ r :: [[String]]) of
  [[_, num]] -> read num
  _ -> 0

parse :: String -> (Integer, [[Integer]])
parse str = do
  let matches = head (str =~ "Game ([0-9]+): (.*)") :: [String]
  let index = read (matches!!1) :: Integer
  let subsets = splitOn "; " (matches!!2)
  let redReg = "([0-9]+) red"
  let greenReg = "([0-9]+) green"
  let blueReg = "([0-9]+) blue"
  let nums = [[numFrom x redReg, numFrom x greenReg, numFrom x blueReg] | x <- subsets]
  (index, nums)

possibilityScoreFrom :: (Integer, [[Integer]]) -> Integer
possibilityScoreFrom (index, nums) = do
  let isPossible = all (\x -> head x <= 12 && x!!1 <= 13 && x!!2 <= 14) nums :: Bool
  if isPossible then index else 0

minCubesIn :: (Integer, [[Integer]]) -> (Integer, Integer, Integer)
minCubesIn (_, nums) = (maximum [head x | x <- nums], maximum [x!!1 | x <- nums], maximum [x!!2 | x <- nums])

powerOf :: (Integer, Integer, Integer) -> Integer
powerOf (r, g, b) = r * g * b

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let parsed = [parse x | x <- ls]
  print (sum [possibilityScoreFrom x | x <- parsed])
  print (sum [(powerOf . minCubesIn) x | x <- parsed])