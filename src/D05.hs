module D05 (execSample, execFinal) where

import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Bifunctor

execSample :: IO ()
execSample = exec "input/sample_05.txt"

execFinal :: IO ()
execFinal = exec "input/05.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let parsed = parseInput ls
  let locationsPart1 = fmap (uncurry (foldl (\s m -> map (applyAnyPart1 m) s))) parsed
  case locationsPart1 of
    Right foo -> print (minimum foo)
    Left str -> print str
  let locationsPart2 = parsed >>= solvePart2 
  case locationsPart2 of
    Right foo -> print foo
    Left str -> print str

data Mapping = Mapping { drs :: Int64, srs :: Int64, rl :: Int64}
  deriving (Read, Show, Eq)

applyPart1 :: Mapping -> Int64 -> Maybe Int64
applyPart1 (Mapping ds ss r) s = if (ss <= s) && (ss + r > s) then Just (ds + s - ss) else Nothing

applyAnyPart1 :: [Mapping] -> Int64 -> Int64
applyAnyPart1 [] i = i
applyAnyPart1 (h : t) i = case applyPart1 h i of
  Just res -> res
  Nothing -> applyAnyPart1 t i

applyPart2 :: [Mapping] -> (Int64, Int64) -> [(Int64, Int64)]
applyPart2 [] t = [t]
applyPart2 ((Mapping ds ss r):other) (seedStart, seedRange) = if (ss >= (seedStart + seedRange)) || (seedStart >= (ss + r))
  then applyPart2 other (seedStart, seedRange)
  else case (ss <= seedStart, (ss + r) >= (seedStart + seedRange)) of
    (True, True) -> [(seedStart - ss + ds, seedRange)]
    (True, False) -> (seedStart - ss + ds, ss + r - seedStart) : applyPart2 other (ss + r, seedRange - ss - r + seedStart)
    (False, True) -> (ds, seedRange - ss + seedStart) : applyPart2 other (seedStart, ss - seedStart)
    (False, False) -> (ds, r) : applyPart2 other (seedStart, ss - seedStart) ++ applyPart2 other (ss + r, seedStart + seedRange - ss - r)

applyAllPart2 :: [[Mapping]] -> (Int64, Int64) -> Int64
applyAllPart2 [] (i, _) = i
applyAllPart2 (m:t) seeds = minimum (map (applyAllPart2 t) (applyPart2 m seeds))

solvePart2 :: ([Int64], [[Mapping]]) -> Either String Int64
solvePart2 (s : r : t, mappings) = do
  let self = applyAllPart2 mappings (s, r)
  if null t
    then Right self
  else fmap (min self) (solvePart2 (t, mappings))
solvePart2 ([], _) = Right (maxBound :: Int64)
solvePart2 ([s], _) = Left ("Ended with unexpected single seed number: " ++ show s)

parseInput :: [String] -> Either String ([Int64], [[Mapping]])
parseInput (seedsLine : maps) = do
  seeds <- (stripPrefix "seeds: " seedsLine ?> "first line not as expected") >>= (mapM readMaybe . words)
  (_, mappings) <- parseMappings maps
  Right (seeds, mappings)
parseInput [] = Left "failed initial parsing : empty"

parseMappings :: [String] -> Either String ([Mapping], [[Mapping]])
parseMappings [] = Right ([], [])
parseMappings ("" : ts) = do
  (h, t) <- parseMappings ts
  Right ([], h : t)
parseMappings (h : t) = case mapM readMaybe (words h) of
  Right [ds, ss, r] -> fmap (Data.Bifunctor.first (Mapping ds ss r :)) (parseMappings t)
  _ -> parseMappings t

readMaybe :: String -> Either String Int64
readMaybe s = case reads s of
  [(x, "")] -> Right x
  _ -> Left ("failed to parse string " ++ s)

(?>) :: Maybe a -> String -> Either String a
Just x ?> _ = Right x
Nothing ?> str = Left str