{-# LANGUAGE ViewPatterns #-}
module D20 (execSample, execFinal) where

import Lib
import Text.Regex.TDFA
import Data.List.Split (splitOn)
import Data.Map (Map, lookup, insert, fromList, empty, foldl)
import Debug.Trace

execSample :: IO ()
execSample = exec "input/sample_20.txt"

execFinal :: IO ()
execFinal = exec "input/20.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path :: IO String
  let ls = lines content :: [String]
  let res = do
        moduleList <- mapM parseModule ls
        let modulesWithoutConj = fromList [(name, Module name typ dest) | Module name typ dest <- moduleList]
        let modules = Data.Map.foldl updateListIfConj modulesWithoutConj modulesWithoutConj
        let (pulseCount, memo) = repeatAndCollect 1000 Data.Map.empty modules
        return (show (scoreFor pulseCount) ++ " | " ++ show (length memo))
  anyways putStrLn res

data ModuleType = Broadcaster | FlipFlop PulseStrength | Conjunction (Map String PulseStrength) deriving (Eq, Show, Ord)
data Module = Module String ModuleType [String] deriving (Eq, Ord)
data PulseStrength = High | Low deriving (Eq, Show, Ord)
data Pulse = Pulse String String PulseStrength deriving (Show)
data PulseCount = PulseCount Int Int deriving (Show)

instance Show Module where
  show (Module _ Broadcaster _) = ""
  show (Module _ (FlipFlop High) _) = "+"
  show (Module _ (FlipFlop Low) _) = "-"
  show (Module _ (Conjunction m) _) = Data.Map.foldl (\str ps -> (if ps == High then '+' else '-') : str) "" m

scoreFor :: PulseCount -> Int
scoreFor (PulseCount a b) = a * b

(+>) :: PulseStrength -> (Map String Module, PulseCount) -> (Map String Module, PulseCount)
(+>) High (modules, PulseCount hi lo) = (modules, PulseCount (hi+1) lo)
(+>) Low (modules, PulseCount hi lo) = (modules, PulseCount hi (lo+1))

updateListIfConj :: Map String Module -> Module -> Map String Module
updateListIfConj modules (Module name (Conjunction _) destinations) = insert name (Module name (Conjunction $ fromList $ zip (inputsFor modules name) (repeat Low)) destinations) modules
updateListIfConj modules _ = modules

neg :: PulseStrength -> PulseStrength
neg High = Low
neg Low = High

repeatAndCollect :: Int -> Map (Map String Module) (Map String Module, PulseCount) -> Map String Module -> (PulseCount, Map (Map String Module) (Map String Module, PulseCount))
repeatAndCollect 0 m _ = (PulseCount 0 0, m)
repeatAndCollect i memo modules =
 let
   (memo', modules', PulseCount lo hi) = trace (show modules) $ memoizedPulsesForButton memo modules
   (PulseCount lo' hi', memof) = repeatAndCollect (i-1) memo' modules'
 in (PulseCount (lo + lo') (hi + hi'), memof)

memoizedPulsesForButton :: Map (Map String Module) (Map String Module, PulseCount) -> Map String Module -> (Map (Map String Module) (Map String Module, PulseCount), Map String Module, PulseCount)
memoizedPulsesForButton memo modules = case Data.Map.lookup modules memo of
  Just (res, c) -> (memo, res, c)
  Nothing -> (newMemo, res, c)
    where
      (res, c) = pulsesFor modules [Pulse "button" "broadcaster" Low]
      newMemo = insert modules (res, c) memo

pulsesFor :: Map String Module -> [Pulse] -> (Map String Module, PulseCount)
pulsesFor modules [] = (modules, PulseCount 0 0)
pulsesFor modules ((Pulse from to strength) : t) = case Data.Map.lookup to modules of
  Just (Module _ Broadcaster destinations) -> strength +> pulsesFor modules (t ++ [Pulse to d strength | d <- destinations])
  Just (Module _ (FlipFlop savedStr) destinations) -> strength +> case (strength, savedStr) of
    (High, _) -> pulsesFor modules t
    (Low, str) -> pulsesFor (insert to (Module to (FlipFlop $ neg str) destinations) modules) (t ++ [Pulse to d (neg str) | d <- destinations])
  Just (Module _ (Conjunction savedInputs) destinations) -> strength +>
    let
      newSavedInputs = insert from strength savedInputs
      newPulse = if all (==High) newSavedInputs then Low else High
      newModules = insert to (Module to (Conjunction newSavedInputs) destinations) modules
    in pulsesFor newModules (t ++ [Pulse to d newPulse | d <- destinations])
  Nothing -> strength +> pulsesFor modules t

inputsFor :: Map String Module -> String -> [String]
inputsFor modules name = Data.Map.foldl (\accum (Module from _ destinations) -> if name `elem` destinations then from : accum else accum) [] modules

parseModule :: String -> Either String Module
parseModule ((=~ "([a-z%&]+) -> (.*)") -> [[_, nat, destinationsStr]]) = do
  let destinations = splitOn ", " destinationsStr
  (name, typ) <- parseModuleNameAndType nat
  Right $ Module name typ destinations
parseModule str = Left $ "Failed to parse module " ++ str

parseModuleNameAndType :: String -> Either String (String, ModuleType)
parseModuleNameAndType "broadcaster" = Right ("broadcaster", Broadcaster)
parseModuleNameAndType ('%' : name) = Right (name, FlipFlop Low)
parseModuleNameAndType ('&' : name) = Right (name, Conjunction Data.Map.empty)
parseModuleNameAndType str = Left $ "Failed to parse module name and type: " ++ str