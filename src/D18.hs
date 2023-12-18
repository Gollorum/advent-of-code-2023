module D18 (execSample, execFinal) where

import Lib
import Text.Regex.TDFA
import Data.Char (isDigit, digitToInt, ord)
import Data.Int(Int64)

execSample :: IO ()
execSample = exec "input/sample_18.txt"

execFinal :: IO ()
execFinal = exec "input/18.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path :: IO String
  let ls = lines content :: [String]
  let res = do
           wrongCommands <- mapM parse ls
           commands <- mapM switch wrongCommands
           let trenches = foldl (\((s,e):t) c -> e +> c : (s,e) : t) [(Pos 0 0, Pos 0 0)] commands
           let pitSize = sum (map doubleAreaWithCenter trenches) + sum [l | (Command _ l _) <- commands]
           return $ show (pitSize `div` 2 + 1)
  anyways putStrLn res

data Dir = U | R | D | L deriving (Eq, Show, Read)
data Command = Command Dir Int64 String deriving (Eq, Show)
data Pos = Pos Int64 Int64 deriving (Eq, Show)

(+>) :: Pos -> Command -> (Pos, Pos)
(+>) (Pos x y) (Command U i _) = (Pos x y, Pos x (y-i))
(+>) (Pos x y) (Command R i _) = (Pos x y, Pos (x+i) y)
(+>) (Pos x y) (Command D i _) = (Pos x y, Pos x (y+i))
(+>) (Pos x y) (Command L i _) = (Pos x y, Pos (x-i) y)

parse :: String -> Either String Command
parse line = case (line =~ "([URDL]) ([0-9]+) \\(#(.+)\\)") :: [[String]] of
  [[_, dir, len, col]] -> Right $ Command (read dir) (read len) col
  res -> Left $ "Failed to parse " ++ line ++ ", result was: " ++ show res

doubleAreaWithCenter :: (Pos, Pos) -> Int64
doubleAreaWithCenter (Pos x0 y0, Pos x1 y1) = x0 * y1 - x1 * y0

switch :: Command -> Either String Command
switch (Command _ _ co) = do
  newLen <- hexToInt $ take 5 co
  newDir <- dirFromCode $ co!!5
  return $ Command newDir newLen co

hexToInt :: String -> Either String Int64
hexToInt chars = fmap (foldl (\s c -> s * 16 + c) 0) (mapM hexDigitToInt chars)

hexDigitToInt :: Char -> Either String Int64
hexDigitToInt c | isDigit c = Right $ fromIntegral $ digitToInt c
hexDigitToInt c | c >= 'a' && c <= 'f' = Right $ fromIntegral $ 10 + ord c - ord 'a'
hexDigitToInt c = Left $ "Failed to parse hex char " ++ [c]

dirFromCode :: Char -> Either String Dir
dirFromCode '0' = Right R
dirFromCode '1' = Right D
dirFromCode '2' = Right L
dirFromCode '3' = Right U
dirFromCode c = Left $ "Unknown direction encountered: " ++ [c]