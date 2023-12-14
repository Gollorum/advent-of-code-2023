{-# LANGUAGE TupleSections #-}

module D14 (execSample, execFinal) where

import Lib (count)
import Debug.Trace
import qualified Data.Map as Map
import Data.Int(Int64)

execSample :: IO ()
execSample = exec "input/sample_14.txt"

execFinal :: IO ()
execFinal = exec "input/14.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let input = parseInput ls
  print (fmap northernLoadWhenNorthed input)
  let res = do
            grid <- input
            let height = length grid
            let width = length (head grid)
            Right (currentNorthernLoad height (allCycles width height 1000000000 (Map.empty, grid)))
  case res of
    Right r -> print r
    Left err -> print err

data Entry = Round | Cube | Empty
  deriving (Eq, Ord)

instance Show Entry where
  show Round = "O"
  show Cube = "#"
  show Empty = "."

toChar :: Entry -> Char
toChar Round = 'O'
toChar Cube = '#'
toChar Empty = '.'

display :: [[Entry]] -> String
display [] = []
display (line : rest) = map toChar line ++ "\n" ++ display rest

parseInput :: [String] -> Either String [[Entry]]
parseInput = mapM (mapM parseEntry)

parseEntry :: Char -> Either String Entry
parseEntry c = case c of
  'O' -> Right Round
  '#' -> Right Cube
  '.' -> Right Empty
  _ -> Left ("Unexpected entry " ++ [c])

allCubeLocationsIn :: [[Entry]] -> Int -> [Int]
allCubeLocationsIn grid x = [y | (y, entry) <- zipWith (\line y -> (y, line!!x)) grid [0..], entry == Cube]

roundsSouthTo :: [[Entry]] -> (Int, Int) -> Int
roundsSouthTo grid (x, y) = count (==Round) (takeWhile (/= Cube) (map (!! x) (drop (y+1) grid)))

northernLoadWhenNorthed :: [[Entry]] -> Int
northernLoadWhenNorthed grid =
  let
    height = length grid
    width = length (head grid)
    blockers = zip [0..(width-1)] (repeat (-1)) ++ concatMap (\x -> map (x,) (allCubeLocationsIn grid x)) [0..(width-1)]
  in sum (map (\(x, y) -> sum (map ((height - y - 1)-) (take (roundsSouthTo grid (x, y)) [0..]))) blockers)

currentNorthernLoad :: Int -> [[Entry]] -> Int
currentNorthernLoad height grid = sum (zipWith (\cost line -> (height - cost) * count (==Round) line) [0..] grid)

tiltAndRotate :: Int -> Int -> [[Entry]] -> [[Entry]]
tiltAndRotate width height grid = map (\x -> reconstruct height (roundsSouthTo grid (x, -1)) (map (\y -> (y, roundsSouthTo grid (x, y))) (allCubeLocationsIn grid x))) [0..(width-1)]

calcFullCycle :: Int -> Int -> [[Entry]] -> [[Entry]]
calcFullCycle width height = tiltAndRotate width height . tiltAndRotate width height . tiltAndRotate width height . tiltAndRotate width height

type Cache = (Map.Map [[Entry]] (Int64, [[Entry]]))

allCycles :: Int -> Int -> Int64 -> (Cache, [[Entry]]) -> [[Entry]]
allCycles _ _ 0 (_, t) = t
allCycles width height remaining (table, grid) = case Map.lookup grid table of
  Just (oldAge, result) -> case remaining `mod` (oldAge - remaining) of
    0 -> grid
    other -> allCycles width height (other - 1) (table, result)
  Nothing ->
    let 
      result = calcFullCycle width height grid
      newMap = Map.insert grid (remaining, result) table
    in allCycles width height (remaining - 1) (newMap, result)

--durationUntilUnchange :: Eq b => ((a, b) -> (a, b)) -> (a, b) -> Int
--durationUntilUnchange f (a, b) =
--  let (newGrid, newB) = f (a, b)
--  in if newB == b then 0 else 1 + durationUntilUnchange f (newGrid, newB)
--  
reconstruct :: Int -> Int -> [(Int, Int)] -> [Entry]
reconstruct height initRounds blockers =
  let
    first = replicate initRounds Round
    (endedAt, others) = foldl (\(lastI, total) (selfI, size) -> (selfI + size + 1, replicate size Round ++ [Cube] ++ replicate (selfI - lastI) Empty ++ total)) (initRounds, first) blockers
  in replicate (height - endedAt) Empty ++ others