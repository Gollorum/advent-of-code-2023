module D09 (execSample, execFinal) where

import Data.List

execSample :: IO ()
execSample = exec "input/sample_09.txt"

execFinal :: IO ()
execFinal = exec "input/09.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let scans = map (map read . words) ls :: [[Int]]
  print ((sum . map extrapolate) scans)
  print ((sum . map (extrapolate . reverse)) scans)

extrapolate :: [Int] -> Int
extrapolate zeros | all (==0) zeros = 0
extrapolate numbers = last numbers + extrapolate (diffsFrom numbers)

diffsFrom :: [Int] -> [Int]
diffsFrom (first : second : rest) = (second - first) : diffsFrom (second : rest)
diffsFrom _ = []