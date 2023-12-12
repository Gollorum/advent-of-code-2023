module D10 (execSample, execFinal) where

import Data.List (elemIndex, findIndex, find)
import Data.Either (isRight)
import Data.Set (Set, singleton, insert)

execSample :: IO ()
execSample = exec "input/sample_10.txt"

execFinal :: IO ()
execFinal = exec "input/10.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let res = do
            grid <- mapM (mapM parse) ls
            y <- case findIndex (elem Start) grid of
              Just i -> Right i
              Nothing -> Left "Cannot find Y!"
            x <- case elemIndex Start (grid!!y) of
              Just i -> Right i
              Nothing -> Left "Cannot find X!"
            let start = (x, y)
            (next, dir) <- case find (\(o, d) -> isRight (pipeAt (start +> o) grid >>= nextDir d (-1, -1))) [((0, -1), N), ((1, 0), E), ((0, 1), S)] of
              Just n -> Right n
              Nothing -> Left "Failed to find first connected"
            loop <- detectLoop (start +> next) dir grid
            let loopLength = length loop
            loopsToTheLeft <- mapM (\y -> loopElemsToTheLeft [(x, y) | x <- [0..(length (grid!!y) - 1)]] loop grid) [0..(length grid - 1)]
            let insideLoop = length [s | (s, e) <- concat loopsToTheLeft, s == Inside && e == Other]
            Right (loopLength `div` 2, insideLoop)
  print res

data Dir = N | E | S | W
  deriving (Eq, Show)

data Pipe = NS | NE | EW | ES | SW | WN | G | Start
  deriving (Eq, Show)

data RayStatus = Inside | Outside
  deriving (Eq)

rev :: RayStatus -> RayStatus
rev Inside = Outside
rev Outside = Inside

data LoopStatus = VerticalLoop | CurveFromSouth | CurveFromNorth | Other
  deriving (Eq)

instance Show RayStatus where
  show Inside = "I"
  show Outside = "O"

instance Show LoopStatus where
  show Other = "."
  show VerticalLoop = "|"
  show CurveFromSouth = "F"
  show CurveFromNorth = "L"

(+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(+>) (a, b) (c, d) = (a + c, b + d)

nextDir :: Dir -> (Int, Int) -> Pipe -> Either String Dir
nextDir N _ NS = Right N
nextDir N _ ES = Right E
nextDir N _ SW = Right W
nextDir E _ EW = Right E
nextDir E _ SW = Right S
nextDir E _ WN = Right N
nextDir S _ NS = Right S
nextDir S _ NE = Right E
nextDir S _ WN = Right W
nextDir W _ EW = Right W
nextDir W _ ES = Right S
nextDir W _ NE = Right N
nextDir d pos p = Left ("Failed to traverse pipe " ++ show p ++ " in direction " ++ show d ++ " (pos: " ++show pos ++")")

offsetFrom :: Dir -> (Int, Int)
offsetFrom N = (0, -1)
offsetFrom E = (1, 0)
offsetFrom S = (0, 1)
offsetFrom W = (-1, 0)

detectLoop :: (Int, Int) -> Dir -> [[Pipe]] -> Either String (Set (Int, Int))
detectLoop pos dir grid = do
  pipe <- pipeAt pos grid
  case pipe of
    Start -> Right (singleton pos)
    p -> do
      next <- nextDir dir pos p
      let nextPos = pos +> offsetFrom next
      fmap (insert pos) (detectLoop nextPos next grid)

pipeAt :: (Int, Int) -> [[Pipe]] -> Either String Pipe
pipeAt (x, y) grid = if (x < 0) || (y < 0) || (y >= length grid)
  then Left ("Queried pipe at invalid position " ++ show (x, y))
  else
    let row = grid!!y
    in if x >= length row
      then Left ("Queried pipe too far right " ++ show (x, y))
      else Right (row!!x)

loopElemsToTheLeft :: [(Int, Int)] -> Set (Int, Int) -> [[Pipe]] -> Either String [(RayStatus, LoopStatus)]
loopElemsToTheLeft [] _ _ = Right []
loopElemsToTheLeft (p : r) loop grid = do
  rest <- loopElemsToTheLeft r loop grid
  let (wasInside, incoming) = case rest of
                (h : _) -> h
                [] -> (Outside, Other)
  selfPipe <- pipeAt p grid
  let selfLoop = p `elem` loop
  newStatus <- updateStatus selfPipe wasInside incoming selfLoop
  Right (newStatus : rest)

updateStatus :: Pipe -> RayStatus -> LoopStatus -> Bool -> Either String (RayStatus, LoopStatus)
updateStatus _ status _ False = Right (status, Other)
updateStatus Start status _ True = Right (rev status, VerticalLoop)
updateStatus WN status _ True = Right (rev status, CurveFromNorth)
updateStatus SW status _ True = Right (rev status, CurveFromSouth)
updateStatus NS status _ True = Right (rev status, VerticalLoop)
updateStatus ES status CurveFromNorth True = Right (status, VerticalLoop)
updateStatus ES status CurveFromSouth True = Right (rev status, VerticalLoop)
updateStatus NE status CurveFromNorth True = Right (rev status, VerticalLoop)
updateStatus NE status CurveFromSouth True = Right (status, VerticalLoop)
updateStatus EW status dir _ = Right (status, dir)
updateStatus p s l i = Left ("Failed to update " ++ show p ++ " and " ++ show s ++ " and " ++ show l ++ " and " ++ show i)

parse :: Char -> Either String Pipe
parse '|' = Right NS
parse '-' = Right EW
parse 'L' = Right NE
parse 'J' = Right WN
parse '7' = Right SW
parse 'F' = Right ES
parse '.' = Right G
parse 'S' = Right Start
parse c = Left ("Failed to parse char " ++ [c])