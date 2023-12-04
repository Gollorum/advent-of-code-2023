module D04 (execSample, execFinal) where

import Data.List.Split (splitOn)
import Data.Bits (shiftL)

execSample :: IO ()
execSample = exec "input/sample_04.txt"

execFinal :: IO ()
execFinal = exec "input/04.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let cardScores = mapM scoreOfLine ls :: Maybe [Int]
  case cardScores of
    Just scores -> do
      print (sum (map part1ScoreFor scores))
      print (sum (totalNumberOfCards (zip (repeat 1) scores)))
    Nothing -> print "nope"

totalNumberOfCards :: [(Int, Int)] -> [Int]
totalNumberOfCards [] = []
totalNumberOfCards ((num, score) : t) =
  let
    additional = replicate score num ++ repeat 0
    remaining = zipWith (\(n, s) a -> (n+a, s)) t additional
  in (num : totalNumberOfCards remaining)

part1ScoreFor :: Int -> Int
part1ScoreFor s = if s > 0
  then shiftL 1 (s-1)
  else 0

scoreOfLine :: String -> Maybe Int
scoreOfLine l = do
  card <- parseLine l
  return (scoreOf card)

scoreOf :: ([Int], [Int]) -> Int
scoreOf (winning, mine) = length (filter (`elem` winning) mine)

parseLine :: String -> Maybe ([Int], [Int])
parseLine l = do
  split1 <- case splitOn ":" l of
    [_, relevantPart] -> Just relevantPart
    _ -> Nothing
  (winningStr, myStr) <- case splitOn "|" split1 of
    [w, m] -> Just (w, m)
    _ -> Nothing
  let separateAndFilter = (filter (/= []) . splitOn " ") :: String -> [String]
  let numsFrom = (\str -> [read n | n <- separateAndFilter str]) :: String -> [Int]
  let winningNums = numsFrom winningStr
  let myNums = numsFrom myStr
  return (winningNums, myNums)