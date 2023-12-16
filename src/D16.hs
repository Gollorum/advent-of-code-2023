module D16 (execSample, execFinal) where

import Data.Array
import Lib
import Data.List (singleton)

execSample :: IO ()
execSample = exec "input/sample_16.txt"

execFinal :: IO ()
execFinal = exec "input/16.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let res = do
            originalGrid <- mapM (mapM parse) ls
            let grid = fmap (\t -> (t, BeamState False False False False)) $ arrayFrom originalGrid
            let ((minX, minY), (maxX, maxY)) = bounds grid
            let allEntries = [((minX, y), R) | y <- [minY..maxY]] ++
                             [((maxX, y), L) | y <- [minY..maxY]] ++
                             [((x, minY), D) | x <- [minX..maxX]] ++
                             [((x, maxY), U) | x <- [minX..maxX]]
            let allPossibleEndStates = map (advanceBeams grid) $ map singleton allEntries
--            let finalGrid = advanceBeams grid [((0,0), R)]
--            return $ show2dArray $ fmap (\(_, b) -> b) finalGrid
--            return $ show $ length $ filter isEnergized $ elems finalGrid
            return $ show $ maximum $ map (\g -> length $ filter isEnergized $ elems g) allPossibleEndStates
  anyways putStrLn res

data Tile = Empty | SplitH | SplitV | ReflectS | ReflectB deriving (Eq)
instance Show Tile where
  show Empty = "."
  show SplitH = "|"
  show SplitV = "-"
  show ReflectS = "/"
  show ReflectB = "\\"

parse :: Char -> Either String Tile
parse '.' = Right Empty
parse '|' = Right SplitH
parse '-' = Right SplitV
parse '/' = Right ReflectS
parse '\\' = Right ReflectB
parse other = Left $ "unknown tile: " ++ [other]

data BeamState = BeamState Bool Bool Bool Bool
instance Show BeamState where
  show (BeamState True False True False) = "|"
  show (BeamState False True False True) = "-"
  show (BeamState u r d l) = if u || r || d || l then "#" else "."

data Dir = U | R | D | L

opposite :: Dir -> Dir
opposite U = D
opposite D = U
opposite R = L
opposite L = R

arrayFrom :: [[a]] -> Array (Int, Int) a
arrayFrom rows =
  let
    height = length rows
    width = length $ head rows
    elementsWithIndex = do
                      (y, row) <- zip [0..] rows
                      (x, e) <- zip [0..] row
                      return ((x, y), e)
  in array ((0, 0), (width-1, height-1)) elementsWithIndex

show2dArray :: Show a => Array (Int, Int) a -> String
show2dArray arr =
  let
    ((minX, minY), (maxX, maxY)) = bounds arr
  in foldl (\res y -> res ++ (foldl (\l x -> l ++ show (arr!(x, y))) "" [minY..maxY]) ++ "\n") "" [minX..maxX]

isEnergizedFrom :: BeamState -> Dir -> Bool
isEnergizedFrom (BeamState ret _ _ _) D = ret
isEnergizedFrom (BeamState _ ret _ _) L = ret
isEnergizedFrom (BeamState _ _ ret _) U = ret
isEnergizedFrom (BeamState _ _ _ ret) R = ret

isEnergized :: (Tile, BeamState) -> Bool
isEnergized (_, BeamState u r d l) = u || r || d || l

advanceBeams :: (Array (Int, Int) (Tile, BeamState)) -> [((Int, Int), Dir)] -> (Array (Int, Int) (Tile, BeamState))
advanceBeams grid [] = grid
advanceBeams grid ((pos, _) : t) | not (isInBounds pos grid) = advanceBeams grid t
advanceBeams grid ((pos, dir) : t) =
  let
    (tile, currentState) = grid!pos
  in if isEnergizedFrom currentState dir
    then advanceBeams grid t
    else let (newGrid, additionalDirs) = advance grid pos dir tile currentState
      in advanceBeams newGrid (additionalDirs ++ t)

advance :: (Array (Int, Int) (Tile, BeamState)) -> (Int, Int) -> Dir -> Tile -> BeamState -> ((Array (Int, Int) (Tile, BeamState)), [((Int, Int), Dir)])
advance grid pos dir tile currentState =
  let
    next = nextDirs tile dir
    newState = foldr energize currentState ((opposite dir) : next)
  in (grid // [(pos, (tile, newState))], map (\d -> (step d pos, d)) next)

energize :: Dir -> BeamState -> BeamState
energize U (BeamState _ r d l) = BeamState True r d l
energize R (BeamState u _ d l) = BeamState u True d l
energize D (BeamState u r _ l) = BeamState u r True l
energize L (BeamState u r d _) = BeamState u r d True

step :: Dir -> (Int, Int) -> (Int, Int)
step U (x, y) = (x, y-1)
step R (x, y) = (x+1, y)
step D (x, y) = (x, y+1)
step L (x, y) = (x-1, y)

nextDirs :: Tile -> Dir -> [Dir]
nextDirs Empty dir = [dir]
nextDirs SplitH L = [U, D]
nextDirs SplitH R = [U, D]
nextDirs SplitH dir = [dir]
nextDirs SplitV U = [L, R]
nextDirs SplitV D = [L, R]
nextDirs SplitV dir = [dir]
nextDirs ReflectS U = [R]
nextDirs ReflectS R = [U]
nextDirs ReflectS D = [L]
nextDirs ReflectS L = [D]
nextDirs ReflectB U = [L]
nextDirs ReflectB R = [D]
nextDirs ReflectB D = [R]
nextDirs ReflectB L = [U]