{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module D25 (execSample, execFinal) where

import Lib
import Text.Regex.TDFA
import Data.List (nub, findIndex)
import Data.Map (Map, fromList, (!), adjust, delete, insert)
import System.Random
import Debug.Trace

execSample :: IO ()
execSample = exec "input/sample_25.txt"

execFinal :: IO ()
execFinal = exec "input/25.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let res = do
            edges <- concatMapM parseEdges ls
            let nodes = nub $ allNodes edges :: [String]

            let edgeMap = dualEdgeMapFrom2 nodes edges
            let cutSet = findCutSetOfSize 3 nodes edgeMap (mkStdGen 1337)
            
            let edgeMap' = foldl remove (fmap (map target) edgeMap) cutSet
            let subtreeSizeses = subtreeSizes edgeMap' nodes

            return $ show cutSet ++ " | " ++ show subtreeSizeses ++ " | result: " ++ show (product subtreeSizeses)
  anyways putStrLn res

data Edge = Edge {target :: String, mergedFrom :: [(String, String)]} deriving (Eq, Show)
toEdge :: String -> String -> Edge
toEdge from to = Edge to [(from, to)]

parseEdges :: String -> Either String [(String, String)]
parseEdges ((=~ "([a-z]+): ([a-z ]+)") -> [[_, from, oth]]) = Right $ map (from, ) (words oth)
parseEdges str = Left $ "Failed to parse " ++ str

destFrom2 :: String -> (String, String) -> [Edge]
destFrom2 from (f, to) | from == f = [toEdge from to]
destFrom2 from (to, f) | from == f = [toEdge from to]
destFrom2 _ _ = []

dualEdgeMapFrom2 :: [String] -> [(String, String)] -> Map String [Edge]
dualEdgeMapFrom2 nodes edges = trace (show nodes) $ fromList $ map (\from -> (from, concatMap (destFrom2 from) edges)) nodes

findCutSetOfSize :: RandomGen g => Int -> [String] -> Map String [Edge] -> g -> [(String, String)]
findCutSetOfSize size nodes edgeMap g =
  let (cutSet, g') = reduceRandomAndReturnCutSet nodes edgeMap g
  in if length cutSet == size then cutSet else trace ("Found non-min cutset: " ++ show cutSet) $ findCutSetOfSize size nodes edgeMap g'

reduceRandomAndReturnCutSet :: RandomGen g => [String] -> Map String [Edge] -> g -> ([(String, String)], g)
reduceRandomAndReturnCutSet [a, _] edgeMap g = (mergedFrom (head $ edgeMap ! a), g)
reduceRandomAndReturnCutSet nodes edgeMap g =
  let (edgeToRemove, g') = selectRandomEdge nodes edgeMap g
      (nodes', edgeMap') = mergeEdge nodes edgeMap edgeToRemove
  in reduceRandomAndReturnCutSet nodes' edgeMap' g'

selectRandomEdge :: RandomGen g => [String] -> Map String [Edge] -> g -> ((String, String), g)
selectRandomEdge nodes edgeMap g =
  let (i1, g1) = uniformR (1, length nodes) g
      from = nodes!!(i1 - 1)
      egs = edgeMap!from
      (i2, g2) = uniformR (1, length egs) g1
      Edge to _ = egs!!(i2 - 1)
  in ((from, to), g2)
--  in trace ("Selected egdge " ++ from ++" - " ++ to) $ ((from, to), g2)

mergeEdge :: [String] -> Map String [Edge] -> (String, String) -> ([String], Map String [Edge])
mergeEdge nodes edgeMap (from, to) =
  let fromTo = from ++ to
      newNodes = fromTo : except from (except to nodes)
      edgesFromFrom = filter (\e -> to /= target e) (edgeMap ! from)
      edgesFromTo = filter (\e -> from /= target e) (edgeMap ! to)
      newEdges = mergeEdges from edgesFromFrom to edgesFromTo
      remOld = fmap (filter (\(Edge t _) -> t /= from && t /= to)) . delete from . delete to
      insertNew = insert fromTo newEdges . (\m -> foldl (\m' (Edge t hist) -> adjust (Edge fromTo hist:) t m') m newEdges)
      newMap = insertNew $ remOld edgeMap
  in (newNodes, newMap)

mergeEdges :: String -> [Edge] -> String -> [Edge] -> [Edge]
mergeEdges _ [] _ toE = toE
mergeEdges from (fromH : fromT) to toE = case findIndex (\e -> target e == target fromH) toE of
  Just i -> merge fromH (toE !! i) : mergeEdges from fromT to (toE `exceptAt` i)
  Nothing -> fromH : mergeEdges from fromT to toE

merge :: Edge -> Edge -> Edge
merge (Edge to histA) (Edge _ histB) = Edge to (histA ++ histB)

allNodes :: [(String, String)] -> [String]
allNodes [] = []
allNodes ((a, b) : t) = a : b : allNodes t

cascadeRemove :: Map String [String] -> ([String], [String]) -> [String] -> ([String], [String])
cascadeRemove _ res [] = res
cascadeRemove edges (taken, remaining) (r : tr) =
  let newTaken = filter (`elem` remaining) $ edges ! r
      newRem = filter (`notElem` newTaken) remaining
  in cascadeRemove edges (taken ++ newTaken, newRem) (tr ++ newTaken)

subtreeSizes :: Map String [String] -> [String] -> [Int]
subtreeSizes _ [] = []
subtreeSizes edges (n : nodes) =
  let (taken, remaining) = cascadeRemove edges ([n], nodes) [n]
  in length taken : subtreeSizes edges remaining

remove :: Map String [String] -> (String, String) -> Map String [String]
remove edges (from, to) = adjust (except to) from $ adjust (except from) to edges

except :: Eq a => a -> [a] -> [a]
except _ [] = []
except a (h : t) | a == h = except a t
except a (h : t) = h : except a t

exceptAt :: [a] -> Int -> [a]
exceptAt (_ : res) 0 = res
exceptAt (h : t) i = h : exceptAt t (i - 1)