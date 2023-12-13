module D12 (execSample, execFinal) where

import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Debug.Trace
import qualified Data.Map as Map

execSample :: IO ()
execSample = exec "input/sample_12.txt"

execFinal :: IO ()
execFinal = exec "input/12.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let res = do
          entries <- mapM parseLine ls
          Right (sum (map (\(str, nums) -> debug ("Result of " ++ str ++ ": ") (snd (validOptions2 Map.empty nums (split str)))) (map transformForPart2 entries)))
  print res

debug :: Show a => String -> a -> a
debug str x = trace (str ++ show x) x

split :: String -> [String]
split str = filter (not . null) (splitOn "." str)

validOptions :: MemoizedCache -> [Int] -> [String] -> (MemoizedCache, Int)
validOptions cache (_ : _) [] = (cache, 0)
validOptions cache [] rest = (cache, if any (elem '#') rest then 0 else 1)
validOptions cache (num : nums) (cur : rest) | length cur == num = if '#' `elem` cur
  then validOptions2 cache nums rest
  else andThen (+) (\c -> validOptions2 c nums rest) (\c -> validOptions2 c (num : nums) rest) cache
validOptions cache (num : nums) (cur : rest) | length cur < num = if '#' `notElem` cur then validOptions2 cache (num : nums) rest else (cache, 0)
validOptions cache (num : nums) (cur : rest) =
  let
    (cache2, ignore) = if '#' `elem` cur then (cache, 0) else validOptions2 cache (num : nums) rest
    maxI = length cur - num
    lastPossible = elemIndex '#' cur
    rangeEnd = case lastPossible of
      Just lp -> min lp maxI
      Nothing -> maxI
    canTakeTail = rangeEnd + num == length cur
    midRangeEnd = if canTakeTail then rangeEnd - 1 else rangeEnd
    midPossibilities = if midRangeEnd >= 0
                      then [i | i <- [0..midRangeEnd], cur!!(i+num) == '?']
                      else []
    (cache3, results) = mapWithCache cache2 (\c i -> validOptions2 c nums (drop (i + num + 1) cur : rest)) midPossibilities
    consumeMid = sum results
    (cache4, takeTail) = if canTakeTail then validOptions2 cache3 nums rest else (cache3, 0)
    combined = ignore + takeTail + consumeMid
    in (cache4, combined)
    
andThen :: (a -> b -> c) -> (MemoizedCache -> (MemoizedCache, a)) -> (MemoizedCache -> (MemoizedCache, b)) -> MemoizedCache -> (MemoizedCache, c)
andThen f makeA makeB cache = let
  (cache1, a) = makeA cache
  (cache2, b) = makeB cache1
  in (cache2, f a b)
    
mapWithCache :: MemoizedCache -> (MemoizedCache -> a -> (MemoizedCache, b)) -> [a] -> (MemoizedCache, [b])
mapWithCache cache _ [] = (cache, [])
mapWithCache cache f (h : t) = let
  (newCache, newHead) = f cache h
  (finalCache, newTail) = mapWithCache newCache f t
  in (finalCache, newHead : newTail)


parseLine :: String -> Either String (String, [Int])
parseLine line = case splitOn " " line of
  [str, numbers] -> Right (str, map read (splitOn "," numbers))
  _ -> Left ("Failed to parse line " ++ line)

transformForPart2 :: (String, [Int]) -> (String, [Int])
transformForPart2 (str, nums) = (str ++ "?" ++ str ++ "?" ++ str ++ "?" ++ str ++ "?" ++ str, nums ++ nums ++ nums ++ nums ++ nums)

validOptions2 :: MemoizedCache -> [Int] -> [String] -> (MemoizedCache, Int)
validOptions2 cache nums str = memoize cache (MemoryKey nums str)

type MemoizedCache = (Map.Map MemoryKey Int)

data MemoryKey = MemoryKey {numbersToFit :: [Int], remainingStrings :: [String]}
  deriving (Eq, Ord)

memoize :: MemoizedCache -> MemoryKey -> (MemoizedCache, Int)
memoize table key =
 case Map.lookup key table of
    Just result -> (table, result)
    Nothing ->
      let (cache, result) = validOptions table (numbersToFit key) (remainingStrings key)
          newTable = Map.insert key result cache
      in (newTable, result)