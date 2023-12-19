{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module D19 (execSample, execFinal) where

import Lib
import Data.List.Split (splitOn)
import Text.Regex.TDFA
import Data.Map (Map, fromList, lookup)
import Control.Monad(filterM)
import Data.Int (Int64)
import Data.Maybe (maybeToList)

execSample :: IO ()
execSample = exec "input/sample_19.txt"

execFinal :: IO ()
execFinal = exec "input/19.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path :: IO String
  let ls = lines content :: [String]
  let res = do
           (workflowsStr, partsStr) <- case splitOn [""] ls of
             [l, r] -> Right (l, r)
             _ -> Left "Failed to split input"
           workflows <- mapM parseWorkflow workflowsStr
           parts <- mapM parsePart partsStr
           let workflowMap = Data.Map.fromList [(name, Workflow name rules) | Workflow name rules <- workflows]
           acceptedParts <- filterM (isAccepted workflowMap "in") parts
           let score = sum $ map worthOf acceptedParts
           possibleRanges <- acceptedSubRanges workflowMap (SuperPart (1,4000) (1,4000) (1,4000) (1,4000), "in")
           return $ show score ++ "\n" ++ show (sum $ map numStates possibleRanges)
  anyways putStrLn res

data Category = X | M | A | S deriving (Show)
data Comparison = Greater | Less deriving (Show)
data Rule = Rule Category Comparison Int String deriving (Show)
data Workflow = Workflow String [Rule] deriving (Show)
data Part = Part Int Int Int Int deriving (Show)

data SuperPart = SuperPart (Int, Int) (Int, Int) (Int, Int) (Int, Int)

numStates :: SuperPart -> Int64
numStates (SuperPart (sX, lX) (sM, lM) (sA, lA) (sS, lS)) = fromIntegral (lX - sX + 1) * fromIntegral (lM - sM + 1) * fromIntegral (lA - sA + 1) * fromIntegral (lS - sS + 1)

worthOf :: Part -> Int
worthOf (Part x m a s) = x+m+a+s

isAccepted :: Map String Workflow -> String -> Part -> Either String Bool
isAccepted _ "A" _ = Right True
isAccepted _ "R" _ = Right False
isAccepted workflows workflowKey part = case Data.Map.lookup workflowKey workflows of
  Just (Workflow _ rules) -> isAccepted workflows (head ([dest | Rule cat comp thresh dest <- rules, test cat comp thresh part])) part
  Nothing -> Left $ "Workflow " ++ workflowKey ++" not found"

acceptedSubRanges :: Map String Workflow -> (SuperPart, String) -> Either String [SuperPart]
acceptedSubRanges _ (sp, "A") = Right [sp]
acceptedSubRanges _ (_, "R") = Right []
acceptedSubRanges workflows (part, workflowKey) = case Data.Map.lookup workflowKey workflows of
  Just (Workflow _ rules) -> concatMapM (acceptedSubRanges workflows) (fst $ foldl applyRule ([], Just part) rules)
  Nothing -> Left $ "Workflow " ++ workflowKey ++" not found"

applyRule :: ([(SuperPart, String)], Maybe SuperPart) -> Rule -> ([(SuperPart, String)], Maybe SuperPart)
applyRule (accum, Nothing) _ = (accum, Nothing)
applyRule (accum, Just sp) rule =
  let (yes, no) = branch rule sp
  in (accum ++ maybeToList (fmap (, targetOf rule) yes), no)

targetOf :: Rule -> String
targetOf (Rule _ _ _ targ) = targ

branch :: Rule -> SuperPart -> (Maybe SuperPart, Maybe SuperPart)
branch (Rule cat Less thresh _) sp | fst (rangeFrom cat sp) >= thresh = (Nothing, Just sp)
branch (Rule cat Greater thresh _) sp | snd (rangeFrom cat sp) <= thresh = (Nothing, Just sp)
branch (Rule cat Less thresh _) sp | snd (rangeFrom cat sp) < thresh = (Just sp, Nothing)
branch (Rule cat Greater thresh _) sp | fst (rangeFrom cat sp) > thresh = (Just sp, Nothing)
branch (Rule cat comp thresh dest) sp =
  let
    (rMin, rMax) = rangeFrom cat sp
    ((yesMin, yesMax), (noMin, noMax)) = case comp of
      Less -> ((rMin, thresh - 1), (thresh, rMax))
      Greater -> ((thresh + 1, rMax), (rMin, thresh))
  in (if yesMin <= yesMax then Just $ copyWith cat (yesMin, yesMax) sp else Nothing, if noMin <= noMax then Just $ copyWith cat (noMin, noMax) sp else Nothing)

copyWith :: Category -> (Int, Int) -> SuperPart -> SuperPart
copyWith X x (SuperPart _ m a s) = SuperPart x m a s
copyWith M m (SuperPart x _ a s) = SuperPart x m a s
copyWith A a (SuperPart x m _ s) = SuperPart x m a s
copyWith S s (SuperPart x m a _) = SuperPart x m a s

test :: Category -> Comparison -> Int -> Part -> Bool
test cat Greater thresh part = valFrom cat part > thresh
test cat Less thresh part = valFrom cat part < thresh

valFrom :: Category -> Part -> Int
valFrom X (Part x _ _ _) = x
valFrom M (Part _ m _ _) = m
valFrom A (Part _ _ a _) = a
valFrom S (Part _ _ _ s) = s

rangeFrom :: Category -> SuperPart -> (Int, Int)
rangeFrom X (SuperPart x _ _ _) = x
rangeFrom M (SuperPart _ m _ _) = m
rangeFrom A (SuperPart _ _ a _) = a
rangeFrom S (SuperPart _ _ _ s) = s

parsePart :: String -> Either String Part
parsePart ((=~ "{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)}") -> [[_, x, m, a, s]]) = Right $ Part (read x) (read m) (read a) (read s)
parsePart str = Left $ "Failed to parse part " ++ str

parseWorkflow :: String -> Either String Workflow
parseWorkflow ((=~ "([a-z]+){(.+)}") -> [[_, name, rulesStr]]) = do
  let rules = splitOn "," rulesStr
  rls <- mapM parseRule (init rules)
  let fallback = last rules
  return $ Workflow name (rls ++ [Rule X Greater 0 fallback])
parseWorkflow str = Left $ "Failed to parse workflow " ++ str

parseRule :: String -> Either String Rule
parseRule ((=~ "([xmas])([<>])([0-9]+):(.+)") -> [[_, category, comparison, threshold, target]]) = do
  cat <- parseCategory category
  comp <- parseComparison comparison
  let thresh = read threshold :: Int
  return $ Rule cat comp thresh target
parseRule str = Left $ "Failed to parse rule " ++ str

parseComparison :: String -> Either String Comparison
parseComparison ">" = Right Greater
parseComparison "<" = Right Less
parseComparison str = Left $ "Failed to parse comparison " ++ str

parseCategory :: String -> Either String Category
parseCategory "x" = Right X
parseCategory "m" = Right M
parseCategory "a" = Right A
parseCategory "s" = Right S
parseCategory str = Left $ "Failed to parse category " ++ str