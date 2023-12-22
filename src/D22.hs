{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module D22 (execSample, execFinal) where

import Lib
import Text.Regex.TDFA
import Data.Array
import Data.List (sortBy, nub)
import GHC.Arr (amap)

execSample :: IO ()
execSample = exec "input/sample_22.txt"

execFinal :: IO ()
execFinal = exec "input/22.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let res = do
            bricks <- mapM parseBrick ls
            let sortedBricks = sortBy (\(Brick _ _ minZ0 _, _) (Brick _ _ minZ1 _, _) -> compare minZ0 minZ1) (zip bricks [0..])
            let (minB, maxB) = foldr unionBounds (Pos 0 0, Pos 0 0) bricks
            let empyMap = array (minB, maxB) [(Pos x' y', (0, -1)) | (x', y') <- concatMap (\y' -> map (, y') [(x minB)..(x maxB)]) [(y minB)..(y maxB)]] :: Array Pos (Int, Int)
            let (_, dependencies) = foldl putOnTop (empyMap, array (0,length bricks - 1) $ zip [0..] (replicate (length bricks) [])) sortedBricks
            let freeCout = count (null . solelyDependentOn dependencies) (indices dependencies)
            let iDependOn = dependencies
            return $ show freeCout ++ " | " ++ (show . sum) (map (\i -> snd (deathsWhenRemoving [i] iDependOn) - 1) [0..(length bricks - 1)])
  anyways putStrLn res

data Brick = Brick Pos Pos Int Int deriving (Show, Eq)
x :: Pos -> Int
x (Pos x' _) = x'
y :: Pos -> Int
y (Pos _ y') = y'


deathsWhenRemoving :: [Int] -> Array Int [Int] -> (Array Int [Int], Int)
deathsWhenRemoving [] dependencies = (dependencies, 0)
deathsWhenRemoving (i : t) dependencies =
  let newDeps = amap (remove i) dependencies
      newDeaths = filter (\io -> dependencies ! io == [i]) (indices dependencies)
      (tailDeps, tailDeaths) = deathsWhenRemoving (t ++ newDeaths) newDeps
  in (tailDeps, tailDeaths + 1)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove a (h : t) = if h == a then remove a t else h : remove a t

solelyDependentOn :: Array Int [Int] -> Int -> [Int]
solelyDependentOn dependencies base = filter (\i -> (dependencies!i) == [base]) (indices dependencies)

putOnTop :: (Array Pos (Int, Int), Array Int [Int]) -> (Brick, Int) -> (Array Pos (Int, Int), Array Int [Int])
putOnTop (heightMap, dependencies) (b, i) =
  let cubes = cubesIn b
      yOffset = maximum $ map (\p -> fst (heightMap ! p)) cubes :: Int
      myDeps = nub [i' | (h, i') <- map (heightMap !) cubes, h == yOffset]
      newDeps = dependencies // [(i, myDeps)]
      newHeightMap = heightMap // map (, (yOffset + 1 + heightOf b, i)) cubes
  in (newHeightMap, newDeps)

cubesIn :: Brick -> [Pos]
cubesIn (Brick mi ma _ _) = do
  x' <- [(x mi)..(x ma)]
  y' <- [(y mi)..(y ma)]
  return $ Pos x' y'

heightOf :: Brick -> Int
heightOf (Brick _ _ _ height) = height

compose :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
compose f a b = Pos (f (x a) (x b)) (f (y a) (y b))

unionBounds :: Brick -> (Pos, Pos) -> (Pos, Pos)
unionBounds (Brick minBrick maxBrick _ _) (minBounds, maxBounds) = (compose min minBrick minBounds, compose max maxBrick maxBounds)

parseBrick :: String -> Either String Brick
parseBrick ((=~ "([0-9]+),([0-9]+),([0-9]+)~([0-9]+),([0-9]+),([0-9]+)") -> [[_, x0, y0, z0, x1, y1, z1]]) =
  let minX = read x0
      minY = read y0
      minZ = read z0
      maxX = read x1
      maxY = read y1
      maxZ = read z1
      minB = Pos (min minX maxX) (min minY maxY)
      maxB = Pos (max minX maxX) (max minY maxY)
      height = max minZ maxZ - min minZ maxZ
  in Right $ Brick minB maxB (min minZ maxZ) height
parseBrick str = Left $ "Failed to parse brick " ++ str