module D23 (execSample, execFinal) where

import Lib
import Data.Array as A
import Safe (maximumMay)
import Data.Maybe (mapMaybe)
import Data.Map as M (Map(..), (!), fromList)
import Debug.Trace (trace)

execSample :: IO ()
execSample = exec "input/sample_23.txt"

execFinal :: IO ()
execFinal = exec "input/23.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let res = do
            tiles <- mapM (mapM parseTile) ls
            let tileMap = arrayFrom tiles
            let (Pos minX minY, Pos maxX maxY) = bounds tileMap
            let start = head $ filter (\p -> (tileMap A.! p) == Path) [Pos x minY | x <- [minX..maxX]]
            let destination = head $ filter (\p -> (tileMap A.! p) == Path) [Pos x maxY | x <- [minX..maxX]]

            let pathPart1 = fmap (\(PathInfo _ _ l) -> l) $ findLongestPathTo tileMap destination $ PathInfo start [] 0

            let mapWithoutSlopes = fmap noSlope tileMap
            let nodes = intersections mapWithoutSlopes ++ [start, destination]
            let edges = M.fromList $ map (\n -> (n, edgesFor mapWithoutSlopes nodes n)) nodes
            let pathPart2 = fmap (\(PathInfo _ _ l) -> l) $ findLongestGraphPathTo nodes edges destination $ PathInfo start [] 0

            return $ show pathPart1 ++ " | " ++ show pathPart2
  anyways putStrLn res

data Tile = Path | SlopeU | SlopeR | SlopeD | SlopeL | Hedge deriving (Eq)
data PathInfo = PathInfo Pos [Pos] Int deriving (Eq, Show)

noSlope :: Tile -> Tile
noSlope SlopeU = Path
noSlope SlopeR = Path
noSlope SlopeD = Path
noSlope SlopeL = Path
noSlope oth = oth

instance Ord PathInfo where
  compare (PathInfo _ _ l0) (PathInfo _ _ l1) = compare l0 l1

instance Show Tile where
  show Path = "."
  show SlopeU = "^"
  show SlopeR = ">"
  show SlopeD = "v"
  show SlopeL = "<"
  show Hedge = "#"

(+>) :: Pos -> (Int, Int) -> Pos
(+>) (Pos x y) (xo, yo) = Pos (x+xo) (y+yo)

parseTile :: Char -> Either String Tile
parseTile '.' = Right Path
parseTile 'v' = Right SlopeD
parseTile '>' = Right SlopeR
parseTile '^' = Right SlopeU
parseTile '<' = Right SlopeL
parseTile '#' = Right Hedge
parseTile c = Left $ "Failed to parse " ++ [c]

nextOptions :: Array Pos Tile -> PathInfo -> [Pos]
nextOptions _ (PathInfo pos history _) | pos `elem` history = []
nextOptions tileMap (PathInfo pos history _) = case tileMap ? pos of
  Nothing -> []
  Just Hedge -> []
  Just SlopeD -> [pos +> (0, 1)]
  Just SlopeR -> [pos +> (1, 0)]
  Just SlopeU -> [pos +> (0, -1)]
  Just SlopeL -> [pos +> (-1, 0)]
  Just Path -> filter (`notElem` take 2 history) $ map (pos +>) [(0, 1), (1, 0), (0, -1), (-1, 0)]

isValid :: Array Pos Tile -> PathInfo -> Pos -> Bool
isValid tileMap (PathInfo _ history _) pos = case tileMap ? pos of
  Just Hedge -> False
  Just _ -> pos `notElem` history
  Nothing -> False

findLongestPathTo :: Array Pos Tile -> Pos -> PathInfo -> Maybe PathInfo
findLongestPathTo _ destination (PathInfo curPos history len) | curPos == destination = Just $ PathInfo curPos history len
findLongestPathTo tileMap destination (PathInfo curPos history len) = maximumMay (do
  nextPos <- filter (isValid tileMap (PathInfo curPos history len)) (nextOptions tileMap (PathInfo curPos history len))
  let res = findLongestPathTo tileMap destination (PathInfo nextPos (curPos : history) (len + 1))
  case res of
    Just r -> [r]
    Nothing -> [])

findLongestGraphPathTo :: [Pos] -> Map Pos [(Pos, Int)] -> Pos -> PathInfo -> Maybe PathInfo
findLongestGraphPathTo _ _ destination (PathInfo curPos history len) | curPos == destination = Just $ PathInfo curPos history len
findLongestGraphPathTo nodes edges destination (PathInfo curPos history len) = maximumMay (do
  (nextPos, nextCost) <- filter (\(p, _) -> p `notElem` history && p /= curPos) (edges M.! curPos)
  let res = findLongestGraphPathTo nodes edges destination (PathInfo nextPos (curPos : history) (len + nextCost))
  case res of
    Just r -> [r]
    Nothing -> [])

intersections :: Array Pos Tile -> [Pos]
intersections tileMap = filter (\p -> isPath tileMap p && count (isPath tileMap) (map (p +>) [(0, 1), (1, 0), (0, -1), (-1, 0)]) > 2) (indices tileMap)

isPath :: Array Pos Tile -> Pos -> Bool
isPath tileMap pos = case tileMap ? pos of
  Just Path -> True
  _ -> False

edgesFor :: Array Pos Tile -> [Pos] -> Pos -> [(Pos, Int)]
edgesFor tileMap nodes node = mapMaybe (findOther tileMap nodes [node]) (map (node +>) [(0, 1), (1, 0), (0, -1), (-1, 0)])

findOther :: Array Pos Tile -> [Pos] -> [Pos] -> Pos -> Maybe (Pos, Int)
findOther _ nodes history pos | pos `elem` nodes = Just (pos, length history)
findOther tileMap nodes history pos = case filter (\p -> isPath tileMap p && p `notElem` history) (map (pos +>) [(0, 1), (1, 0), (0, -1), (-1, 0)]) of
  [next] -> findOther tileMap nodes (pos : history) next
  _-> Nothing