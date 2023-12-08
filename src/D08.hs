module D08 (execSample, execFinal) where

import Data.Map (Map, fromList, lookup, keys)
import Data.Int(Int64)
import Text.Regex.TDFA
import Data.List

execSample :: IO ()
execSample = exec "input/sample_08.txt"

execFinal :: IO ()
execFinal = exec "input/08.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let input = parseInput ls
  case input >>= numStepsToGoalPart1 "AAA" of
    Right res -> print res
    Left err -> print err
  let withDirLen = fmap (\(dirs, graph) -> (length dirs, cycle dirs, graph)) input
  case withDirLen >>= allCycleInputs of
    Right res -> if all (\ci -> all (==offset ci)(targetsInCycle ci)) res
      then print (foldl lcm (1 :: Int64) (map (fromIntegral . offset) res))
      else print ":("
    Left err -> print err

data Dir = L | R
  deriving (Show)

data Node = Node {l :: String, r :: String}
  deriving (Show)

data CycleInfo = CycleInfo {offset :: Int, targetsInCycle :: [Int], startingNode :: String}
  deriving (Show)

numStepsToGoalPart1 :: String -> ([Dir], Map String Node) -> Either String Int
numStepsToGoalPart1 "ZZZ" _ = Right 0
numStepsToGoalPart1 node (dir : nextDirs, graph) = do
  nextNode <- step (dir, graph) node
  furtherSteps <- numStepsToGoalPart1 nextNode (nextDirs, graph)
  Right (furtherSteps + 1)
numStepsToGoalPart1 _ _ = Left "no directions?"

allCycleInputs :: (Int, [Dir], Map String Node) -> Either String [CycleInfo]
allCycleInputs (dirCount, dirs, graph) = mapM (cycleInfoFor (dirCount, dirs, graph)) (filter (\n -> n!!2 == 'A') (Data.Map.keys graph))

cycleInfoFor :: (Int, [Dir], Map String Node) -> String -> Either String CycleInfo
cycleInfoFor info node = do
  (_, i, path) <- stepUntilCycle info [node]
  let cycleP = reverse (take i path)
  let offsetOrig = length path - i
  let (rotatedPath, offset) = offsetPath cycleP offsetOrig
  let distances = collectGoalDistances (reverse rotatedPath) [1]
  Right (CycleInfo offset distances node)

offsetPath :: [String] -> Int -> ([String], Int)
offsetPath (h : rest) offset | h!!2 /= 'Z' = offsetPath (rest ++ [h]) (offset + 1)
offsetPath (h : rest) offset = (rest, offset)

collectGoalDistances :: [String] -> [Int] -> [Int]
collectGoalDistances [] distances = distances
collectGoalDistances (h : t) (hn : tn) = if h!!2 == 'Z'
  then collectGoalDistances t (1 : hn : tn)
  else collectGoalDistances t ((hn + 1) : tn)
collectGoalDistances _ [] = [-1]

stepUntilCycle :: (Int, [Dir], Map String Node) -> [String] -> Either String (String, Int, [String])
stepUntilCycle (dirCount, dir : nextDirs, graph) (current : history) = case cycleLengthFrom dirCount current history of
  Just cycleLen -> Right (current, cycleLen, history)
  Nothing -> do
                next <- step (dir, graph) current
                stepUntilCycle (dirCount, nextDirs, graph) (next : current : history)
stepUntilCycle _ _ = Left "Failed to stepUntilCycle"

cycleLengthFrom :: Int -> String -> [String] -> Maybe Int
cycleLengthFrom dirCount current history = case filter (\i -> (i `mod` dirCount) == 0) (map (+1) (elemIndices current history)) of
  [i] -> Just i
  _ -> Nothing

step :: (Dir, Map String Node) -> String -> Either String String
step (dir, graph) node = do
  n <- case Data.Map.lookup node graph of
    Just n -> Right n
    Nothing -> Left ("Failed to find node " ++ node ++ " in graph")
  let nextNode = case dir of
        L -> l n
        R -> r n
  Right nextNode

parseInput :: [String] -> Either String ([Dir], Map String Node)
parseInput (directionsStr : "" : nodesStr) = do
  dirs <- mapM parseDirection directionsStr
  nodes <- mapM parseNode nodesStr
  Right (dirs, fromList nodes)
parseInput other = Left ("Failed to parse input: " ++ show other)

parseDirection :: Char -> Either String Dir
parseDirection 'L' = Right L
parseDirection 'R' = Right R
parseDirection c = Left ("Unknown direction char " ++ [c])

parseNode :: String -> Either String (String, Node)
parseNode line = case (line =~ "([A-Z0-9]+) = \\(([A-Z0-9]+), ([A-Z0-9]+)\\)") :: [[String]] of
  [[_, from, left, right]] -> Right (from, Node left right)
  oth -> Left ("Failed to parse line " ++ line ++ ": " ++ show oth)