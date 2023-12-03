module D03 (execSample, execFinal) where

import Data.Char (isDigit)
import Control.Monad (guard)

execSample :: IO ()
execSample = exec "input/sample_03.txt"

execFinal :: IO ()
execFinal = exec "input/03.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let symbols = collectSymbolLocations ls
  let numbers = collectNumbers ls
  let adjacentNums = filter (\(b, n) -> any (containsPos b) [fst x | x <- symbols]) numbers
  print (sum [snd x | x <- adjacentNums])
  print (sum [gearRatioOf x numbers | x <- symbols])

data Pos = Pos {x :: Int, y :: Int}
  deriving (Read, Show, Eq)

data Bounds = Bounds {min :: Pos, max :: Pos}
    deriving (Read, Show, Eq)

containsPos :: Bounds -> Pos -> Bool
containsPos (Bounds (Pos xMin yMin) (Pos xMax yMax)) pos = xMin <= x pos && yMin <= y pos && xMax >= x pos && yMax >= y pos

collectSymbolLocations :: [String] -> [(Pos, Char)]
collectSymbolLocations lines = do
  (y, line) <- zip [0..] lines
  (x, char) <- zip [0..] line
  guard (not (char == '.' || isDigit char))
  return (Pos x y, char)

collectNumbers :: [String] -> [(Bounds, Int)]
collectNumbers lines = do
  (y, line) <- zip [0..] lines
  (x, char) <- zip [0..] line
  guard (isDigit char && (x == 0 || not (isDigit (line!!(x-1)))))
  let numStr = takeWhile isDigit (drop x line)
  let num = read numStr :: Int
  let b = Bounds (Pos (x-1) (y-1)) (Pos (x+length numStr) (y+1)) :: Bounds
  return (b, num)

gearRatioOf :: (Pos, Char) -> [(Bounds, Int)] -> Int
gearRatioOf (p, '*') = \numbers -> case filter (\(b, _) -> containsPos b p) numbers of
  [(_, a), (_, b)] -> a * b
  _ -> 0
gearRatioOf _ = const 0