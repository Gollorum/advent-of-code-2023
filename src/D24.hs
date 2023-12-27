{-# LANGUAGE ViewPatterns #-}
module D24 (execSample, execFinal, execSamplePart2, execFinalPart2) where

import Lib
import Text.Regex.TDFA
import Math.MFSolve
import Numeric (showFFloat)

execSample :: IO ()
execSample = exec "input/sample_24.txt" 7 27

execFinal :: IO ()
execFinal = exec "input/24.txt" 200000000000000 400000000000000

execSamplePart2 :: IO ()
execSamplePart2 = execP2 "input/sample_24.txt"

execFinalPart2 :: IO ()
execFinalPart2 = execP2 "input/24.txt"

exec :: String -> Double -> Double -> IO ()
exec path minB maxB = do
  content <- readFile path
  let ls = lines content
  let res = do
            particles <- mapM parse ls
            let numIntersect = count (doCollideXY minB maxB) (allPairs particles)
            return $ show numIntersect
  anyways putStrLn res

execP2 :: String -> IO ()
execP2 path = do
  content <- readFile path
  let ls = lines content
  let Right particles = mapM parse ls
  let (_ : _ : p1 : p2 : p3 : p4 : _) = map (fmap makeConstant) particles :: [VarParticle]
  let vars = map SimpleVar ["px", "py", "pz", "vx", "vy", "vz", "result"]
  let [px, py, pz, vx, vy, vz, res] = map makeVariable vars :: [VarT]
  let p0 = AParticle (APos3 px py pz) (APos3 vx vy vz) :: VarParticle
  
  let di = (\pi -> pi - p0) :: VarParticle -> VarParticle
  let dpi = (\pi -> pos pi - pos p0) :: VarParticle -> VarPos
  let dvi = (\pi -> vel pi - vel p0) :: VarParticle -> VarPos
  let cros = (\a b p q -> a (pos p) * b (vel q) - b (pos p) * a (vel q))
  -- pi x vi - p0 x vi - pi x v0 + p0 x v0 = 0
  let Right deps = flip execSolver noDeps $ do
                                             cros y z p2 p2 - cros y z p0 p2 - cros y z p2 p0 === cros y z p1 p1 - cros y z p0 p1 - cros y z p1 p0
                                             cros z x p2 p2 - cros z x p0 p2 - cros z x p2 p0 === cros z x p1 p1 - cros z x p0 p1 - cros z x p1 p0
                                             cros x y p2 p2 - cros x y p0 p2 - cros x y p2 p0 === cros x y p1 p1 - cros x y p0 p1 - cros x y p1 p0
                                             cros y z p4 p4 - cros y z p0 p4 - cros y z p4 p0 === cros y z p3 p3 - cros y z p0 p3 - cros y z p3 p0
                                             cros z x p4 p4 - cros z x p0 p4 - cros z x p4 p0 === cros z x p3 p3 - cros z x p0 p3 - cros z x p3 p0
                                             cros x y p4 p4 - cros x y p0 p4 - cros x y p4 p0 === cros x y p3 p3 - cros x y p0 p3 - cros x y p3 p0
                                             res === px + py + pz
  mapM (\v -> case getKnown v deps of
    Left _ -> putStrLn $ "I don't know " ++ show v
    Right va -> putStrLn $ show v ++ " = " ++ Numeric.showFFloat Nothing va ""
    ) vars
  return ()
  
instance Functor APos3 where
  fmap f (APos3 x' y' z') = APos3 (f x') (f y') (f z')

instance Functor AParticle where
  fmap f (AParticle p v) = AParticle (fmap f p) (fmap f v)
  
instance Num a => Num (APos3 a) where
  (+) = map2 (+)
  (-) = map2 (-)
  (*) = map2 (*)
  abs = fmap abs
  signum = fmap signum
  
instance Num a => Num (AParticle a) where
  (+) = map2 (+)
  (-) = map2 (-)
  (*) = map2 (*)
  abs = fmap abs
  signum = fmap signum
  
class MegaFunctor f where
  map2 :: (a -> a -> b) -> f a -> f a -> f b
  
instance MegaFunctor APos3 where
  map2 f (APos3 x0 y0 z0) (APos3 x1 y1 z1) = APos3 (f x0 x1) (f y0 y1) (f z0 z1)

instance MegaFunctor AParticle where
  map2 f (AParticle p0 v0) (AParticle p1 v1) = AParticle (map2 f p0 p1) (map2 f v0 v1)

data APos3 a = APos3 {x :: a, y :: a, z :: a}
data AParticle a = AParticle {pos :: APos3 a, vel :: APos3 a}

type Pos3 = APos3 Double
type Particle = AParticle Double

type VarT = Expr SimpleVar Double

type VarPos = APos3 VarT
type VarParticle = AParticle VarT

parse :: String -> Either String Particle
parse ((=~ "([0-9]+), ([0-9]+), ([0-9]+) @ ([- 0-9]+), ([- 0-9]+), ([- 0-9]+)") -> [[_, x, y, z, vx, vy, vz]]) =
  Right $ AParticle (APos3 (read x) (read y) (read z)) (APos3 (read vx) (read vy) (read vz))
parse str = Left $ "Failed to parse particle " ++ str

doCollideXY :: Double -> Double -> (Particle, Particle) -> Bool
doCollideXY _ _ (AParticle _ (APos3 vx0 vy0 _), AParticle _ (APos3 vx1 vy1 _)) | (vx0 / vy0) == (vx1 / vy1) = False
doCollideXY minB maxB (AParticle (APos3 x0 y0 _) (APos3 vx0 vy0 _), AParticle (APos3 x1 y1 _) (APos3 vx1 vy1 _)) =
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