module D15 (execSample, execFinal) where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.List (findIndex)

execSample :: IO ()
execSample = exec "input/sample_15.txt"

execFinal :: IO ()
execFinal = exec "input/15.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let steps = splitOn "," (filter (/='\n') content)
  let hashes = map hash steps
  print (sum hashes)
  let res = do
            operations <- mapM parse steps
            let boxes = replicate 256 [] :: [[Lens]]
            let newBoxes = foldl execute boxes operations
            let power = sum (powerOf newBoxes)
            Right power
  print res

powerOf :: [[Lens]] -> [Int]
powerOf boxes = do
  (box, boxNum) <- zip boxes [1..]
  (Lens _ lensLen, lensNum) <- zip (reverse box) [1..]
  [boxNum * lensNum * lensLen]

hash :: String -> Int
hash = foldl (\s cur -> ((s + ord cur) * 17) `mod` 256) 0

data Operation = Remove String | Insert String Int
  deriving(Show, Eq)

labelOf :: Operation -> String
labelOf (Remove l) = l
labelOf (Insert l _) = l

data Lens = Lens String Int deriving (Show)

parse :: String -> Either String Operation
parse str = case splitOn "=" str of
  [l, fl] -> Right (Insert l (read fl))
  _ -> if '-' == last str then Right (Remove (init str)) else Left ("Failed to parse " ++ str)

execute :: [[Lens]] -> Operation -> [[Lens]]
execute boxes op =
  let
    label = labelOf op
    i = hash label
    oldBox = boxes!!i
    newBox = case op of
      Remove _ -> filter (\(Lens curLab _) -> curLab /= label) oldBox
      Insert _ len -> case findIndex (\(Lens l _) -> l == label) oldBox of
        Just i2 -> (take i2 oldBox) ++ [Lens label len] ++ (drop (i2+1) oldBox)
        Nothing -> (Lens label len) : oldBox
  in (take i boxes) ++ [newBox] ++ (drop (i+1) boxes)