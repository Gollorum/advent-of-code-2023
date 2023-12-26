{-# LANGUAGE ViewPatterns #-}
module D24 (execSample, execFinal) where

import Lib
import Text.Regex.TDFA
import Data.List (sortBy)

execSample :: IO ()
execSample = exec "input/sample_24.txt" 7 27

execFinal :: IO ()
execFinal = exec "input/24.txt" 200000000000000 400000000000000

exec :: String -> Double -> Double -> IO ()
exec path minB maxB = do
  content <- readFile path
  let ls = lines content
  let res = do
            particles <- mapM parse ls
            let numIntersect = count (doCollideXY minB maxB) (allPairs particles)
            let ps = sortBy (\(Particle x _ _ _ _ _) (Particle x2 _ _ _ _ _) -> x `compare` x2) particles
            return $ show ps
  anyways putStrLn res

data Particle = Particle Double Double Double Double Double Double

instance Show Particle where
  show (Particle x y z vx vy vz) = show x ++ ", " ++ show y ++ ", " ++ show z ++ " @ " ++ show vx ++ ", " ++ show vy ++ ", " ++ show vz

parse :: String -> Either String Particle
parse ((=~ "([0-9]+), ([0-9]+), ([0-9]+) @ ([- 0-9]+), ([- 0-9]+), ([- 0-9]+)") -> [[_, x, y, z, vx, vy, vz]]) =
  Right $ Particle (read x) (read y) (read z) (read vx) (read vy) (read vz)
parse str = Left $ "Failed to parse particle " ++ str


doCollideXY :: Double -> Double -> (Particle, Particle) -> Bool
doCollideXY _ _ (Particle _ _ _ vx0 vy0 _, Particle _ _ _ vx1 vy1 _) | (vx0 / vy0) == (vx1 / vy1) = False
doCollideXY minB maxB (Particle x0 y0 _ vx0 vy0 _, Particle x1 y1 _ vx1 vy1 _) =
-- (x0 y0) + r * (vx0 vy0) = (x1 y1) + s * (vx1 vy1)
-- r = (x1 - x0 + s * vx1) / vx0
-- (y1 - y0 + s * vy1) / vy0 = (x1 - x0 + s * vx1) / vx0
-- (y1 - y0 + s * vy1) * vx0 = (x1 - x0 + s * vx1) * vy0
-- (y1-y0)*vx0 + s*vy1*vx0 = (x1-x0)*vy0 + s*vx1*vy0
-- s = ((x1-x0)*vy0 - (y1-y0)*vx0) / (vy1*vx0 - vx1*vy0)
  let s = ((x1-x0)*vy0 - (y1-y0)*vx0) / (vy1*vx0 - vx1*vy0) :: Double
      r = (x1 - x0 + s * vx1) / vx0 :: Double
      x = x0 + r * vx0 :: Double
      y = y0 + r * vy0 :: Double
  in (s >= 0) && (r >= 0) && (x >= minB) && (x <= maxB) && (y >= minB) && (y <= maxB)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (h : t) = zip (repeat h) t ++ allPairs t