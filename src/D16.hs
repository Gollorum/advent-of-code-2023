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
            let ((Pos minX minY), (Pos maxX maxY)) = bounds grid
            let allEntries = [((Pos minX y), R) | y <- [minY..maxY]] ++
                             [((Pos maxX y), L) | y <- [minY..maxY]] ++
                             [((Pos x minY), D) | x <- [minX..maxX]] ++
                             [((Pos x maxY), U) | x <- [minX..maxX]]
            let allPossibleEndStates = map (advanceBeams grid) $ map singleton allEntries
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

isEnergizedFrom :: BeamState -> Dir -> Bool
isEnergizedFrom (BeamState ret _ _ _) D = ret
isEnergizedFrom (BeamState _ ret _ _) L = ret
isEnergizedFrom (BeamState _ _ ret _) U = ret
isEnergizedFrom (BeamState _ _ _ ret) R = ret

isEnergized :: (Tile, BeamState) -> Bool
isEnergized (_, BeamState u r d l) = u || r || d || l

advanceBeams :: Array Pos (Tile, BeamState) -> [(Pos, Dir)] -> Array Pos (Tile, BeamState)
advanceBeams grid [] = grid
advanceBeams grid ((pos, _) : t) | not (isInBounds pos grid) = advanceBeams grid t
advanceBeams grid ((pos, dir) : t) =
  let
    (tile, currentState) = grid!pos
  in if isEnergizedFrom currentState dir
    then advanceBeams grid t
    else let (newGrid, additionalDirs) = advance grid pos dir tile currentState
      in advanceBeams newGrid (additionalDirs ++ t)

advance :: Array Pos (Tile, BeamState) -> Pos -> Dir -> Tile -> BeamState -> (Array Pos (Tile, BeamState), [(Pos, Dir)])
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

step :: Dir -> Pos -> Pos
step U (Pos x y) = (Pos x (y-1))
step R (Pos x y) = (Pos (x+1) y)
step D (Pos x y) = (Pos x (y+1))
step L (Pos x y) = (Pos (x-1) y)

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