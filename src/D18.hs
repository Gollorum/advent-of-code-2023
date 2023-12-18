module D18 (execSample, execFinal) where

import Lib
import Text.Regex.TDFA
import Control.Monad (guard)

execSample :: IO ()
execSample = exec "input/sample_18.txt"

execFinal :: IO ()
execFinal = exec "input/18.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path :: IO String
  let ls = lines content :: [String]
  let res = do
           commands <- mapM parse ls
           let trenches = foldl (\((s,e,o,hd,vd):t) c -> (+>) e hd vd c : (s,e,o,hd,vd) : t) [(Pos 0 0, Pos 0 0, Vert, VD, HL)] commands
           let (Pos minX minY, Pos maxX maxY) = foldl minMax (Pos 0 0, Pos 0 0) [p | (_, p, _, _, _) <- trenches]
           let pitSize = length (do
                                x <- [minX..maxX]
                                y <- [minY..maxY]
                                let p = Pos x y
                                guard $ any (`isOn` p) trenches || ((count (`isLeftOf` p) trenches `mod` 2) == 1)
                                return ())
           return $ show pitSize
--           return (do
--                                y <- [minY..maxY]
--                                x <- [minX..maxX]
--                                let p = Pos x y
----                                let c = if any (`isOn` p) trenches then "#" else "."
----                                let c = show (count (`isLeftOf` p) trenches)
----                                if x == 0 then '\n' : c else c)
--                                let c = if any (`isOn` p) trenches then "#" else show (count (`isLeftOf` p) trenches)
----                                let c = if any (`isOn` p) trenches then "#" else show (count (`isLeftOf` p) trenches `mod` 2)
--                                if x == minX then '\n': c else c)
  anyways putStrLn res

data Dir = U | R | D | L deriving (Eq, Show, Read)
data Orientation = Vert | Horz deriving (Eq, Show)
data VertDir = VU | VD deriving (Eq, Show)
data HorzDir = HR | HL deriving (Eq, Show)
data Command = Command Dir Int String deriving (Eq, Show)
data Pos = Pos Int Int deriving (Eq, Show)

(+>) :: Pos -> VertDir -> HorzDir -> Command -> (Pos, Pos, Orientation, VertDir, HorzDir)
(+>) (Pos x y) vd hd (Command U i _) = (Pos x (if vd == VU then y-1 else y), Pos x (y-i), Vert, VU, hd)
(+>) (Pos x y) vd hd (Command R i _) = (Pos (if hd == HR then x+1 else x) y, Pos (x+i) y, Horz, vd, HR)
(+>) (Pos x y) vd hd (Command D i _) = (Pos x (if vd == VD then y+1 else y), Pos x (y+i), Vert, VD, hd)
(+>) (Pos x y) vd hd (Command L i _) = (Pos (if hd == HL then x-1 else x) y, Pos (x-i) y, Horz, vd, HL)

minMax :: (Pos, Pos) -> Pos -> (Pos, Pos)
minMax (Pos minX minY, Pos maxX maxY) (Pos x y) = (Pos (min minX x) (min minY y), Pos (max maxX x) (max maxY y))

parse :: String -> Either String Command
parse line = case (line =~ "([URDL]) ([0-9]+) \\(#(.+)\\)") :: [[String]] of
  [[_, dir, len, col]] -> Right $ Command (read dir) (read len) col
  res -> Left $ "Failed to parse " ++ line ++ ", result was: " ++ show res

isOn :: (Pos, Pos, Orientation, VertDir, HorzDir) -> Pos -> Bool
isOn (Pos fromX y, Pos toX _, Horz, _, _) (Pos posX posY) = posY == y && (min fromX toX <= posX && max fromX toX >= posX)
isOn (Pos x fromY, Pos _ toY, Vert, _, _) (Pos posX posY) = posX == x && (min fromY toY <= posY && max fromY toY >= posY)

isLeftOf :: (Pos, Pos, Orientation, VertDir, HorzDir) -> Pos -> Bool
isLeftOf (_, _, Horz, _, _) _ = False
isLeftOf (Pos x fromY, Pos _ toY, Vert, _, _) (Pos posX posY) = posX < x && (min fromY toY <= posY && max fromY toY >= posY)