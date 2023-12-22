module D21 (execSample, execFinal, execFinalPart2, execSamplePart2) where

import Lib
import Debug.Trace
import Data.Ix
import Data.Array (Array, array, indices, (!), (//), bounds, elems)
import Data.List (nub)

execSample :: IO ()
execSample = exec "input/sample_21.txt" 6

execFinal :: IO ()
execFinal = exec "input/21.txt" 66

execSamplePart2 :: IO ()
execSamplePart2 = exec "input/sample_21.txt" 27

execFinalPart2 :: IO ()
execFinalPart2 = exec "input/21.txt" 26501365

exec :: String -> Int -> IO ()
exec path stepCount = do
  content <- readFile path :: IO String
  let ls = lines content :: [String]
  let res = do
         elements <- elementsFrom ls
         let bounds = (Pos 0 0, Pos (length (head ls) - 1) (length ls - 1))
         if (xOf . snd) bounds == (yOf . snd) bounds then Right () else Left "bounds don't match"
--         let arr' = array bounds elements
         let empty = array bounds [(i, if e == Occupied then Empty else e) | (i, e) <- elements]
         let pp =  Pos (div (length ls) 2) (div (length ls) 2)
         let arr' = empty // [(pp, Occupied)]
         let lastFound' = [pp]
--         let lastFound' = filter (\i -> arr'!i == Occupied) (indices arr')
--         let (newState', newFound') = repeatStep arr' empty lastFound' stepCount
         let len = length ls
--         let len = trace (showArray newState') $ trace ("784 should be " ++ show (count (==Occupied) (elems newState'))) $ length ls
         let mid = div len 2
         let reach = (div (stepCount - div (len + 1) 2) len) + 1
         let lastSteps = mod (stepCount - div (len + 1) 2) len
         let calcOccupation = \p steps ->
               let arr = empty // [(p, Occupied)]
                   (newState, _) = repeatStep arr empty [p] steps
               in count (==Occupied) (elems newState)
         let spikes = trace ("len: " ++ show len ++ ", mid: " ++ show mid ++", reach: " ++ show reach ++ ", last Steps: " ++show lastSteps) $ (calcOccupation (Pos mid 0) lastSteps) + (calcOccupation (Pos 0 mid) lastSteps) + (calcOccupation (Pos mid (len-1)) lastSteps) + (calcOccupation (Pos (len-1) mid) lastSteps )
         let rr = reach-1
         let filled1 = (rr*rr) * calcOccupation (Pos mid mid) (len+2)
         let filled2 = (reach*reach) * calcOccupation (Pos mid mid) (len+1)
         let lsd = (div lastSteps 2) - 1
         let lsd2 = len + lsd
         let rim1 = trace ("Lsd: " ++ show lsd) $ (reach) * ((calcOccupation (Pos 0 0) lsd) + (calcOccupation (Pos 0 (len-1)) lsd) + (calcOccupation (Pos (len-1) 0) lsd) + (calcOccupation (Pos (len-1) (len-1)) lsd))
         let rim2 = (reach-1) * ((calcOccupation (Pos 0 0) lsd2) + (calcOccupation (Pos 0 (len-1)) lsd2) + (calcOccupation (Pos (len-1) 0) lsd2) + (calcOccupation (Pos (len-1) (len-1)) lsd2))
--         return $ show (spikes + filled + rim)
         return $ show (spikes + filled1 + filled2 + rim1 + rim2) ++ " | spikes: " ++ show spikes ++", filled: " ++ show filled1 ++ ", rim1: " ++ show rim1 ++ ", rim2: " ++ show rim2
--         return $ showArray newState ++ " | " ++ show (count (==Occupied) (elems newState))
  anyways putStrLn res

data Tile = Empty | Rock | Occupied deriving (Eq)
instance Show Tile where
  show Empty = "."
  show Rock = "#"
  show Occupied = "O"

xOf :: Pos -> Int
xOf (Pos x _) = x
yOf :: Pos -> Int
yOf (Pos y _) = y

repeatStep :: Array Pos Tile -> Array Pos Tile -> [Pos] -> Int -> (Array Pos Tile, [Pos])
repeatStep ret _ po 0 = (ret, po)
repeatStep currentState lastState positionsFoundLastTime stepsRemaining = 
  let (newState, newFound) = step currentState lastState positionsFoundLastTime
  in repeatStep newState currentState newFound (stepsRemaining - 1)

step :: Array Pos Tile -> Array Pos Tile -> [Pos] -> (Array Pos Tile, [Pos])
step currentState lastState positionsFoundLastTime = 
  let newPositions = filter (isEmpty currentState) (concatMap neighborsOf positionsFoundLastTime)
  in (lastState // zip newPositions (repeat Occupied), nub newPositions)

elementsFrom :: [String] -> Either String [(Pos, Tile)]
elementsFrom lines = sequence $ do
  (line, y) <- zip lines [0..]
  (el, x) <- zip line [0..]
  return $ case el of
    '#' -> Right (Pos x y, Rock)
    '.' -> Right (Pos x y, Empty)
    'S' -> Right (Pos x y, Occupied)
    o -> Left $ "Failed to parse char " ++ [o]
    
neighborsOf :: Pos -> [Pos]
neighborsOf (Pos x y) = [Pos (x-1) y, Pos x (y-1), Pos (x+1) y, Pos x (y+1)]

isEmpty :: Array Pos Tile -> Pos -> Bool
isEmpty arr pos = case arr ? pos of
  Just Empty -> True
  _ -> False
  
  
showArray :: Show a => Array Pos a -> String
showArray arr =
  let
    (Pos minX minY, Pos maxX maxY) = bounds arr
  in foldl (\res y -> res ++ foldl (\l x -> l ++ show (arr!Pos x y)) "" [minY..maxY] ++ "\n") "" [minX..maxX]
