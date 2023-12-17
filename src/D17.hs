{-# LANGUAGE ViewPatterns #-}

module D17 (execSample, execFinal) where

import Lib
import Data.Char (digitToInt)
import Data.Array
import qualified Data.PQueue.Min as Queue
import Data.PQueue.Min (getMin, deleteFindMin, deleteMin, insert)
import Data.Maybe (mapMaybe)

execSample :: IO ()
execSample = exec "input/sample_17.txt"

execFinal :: IO ()
execFinal = exec "input/17.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path :: IO String
  let ls = lines content :: [String]
  let grid = arrayFrom $ map (map digitToInt) ls :: Array (Int, Int) Int
  let (minB, maxB) = bounds grid
  let queue = Queue.fromList [PathInfo (0,0) 0 (estimatedCost grid (0,0)) Vertical [(0,0)], PathInfo (0,0) 0 (estimatedCost grid (0,0)) Horizontal [(0,0)]] :: Queue.MinQueue PathInfo
  let minA = (minB, Horizontal)
  let maxA = (maxB, Vertical)
  let hasBeenHandled = array (minA, maxA) [(i, False) | i <- enumerate minA maxA]
  print (totalCost grid hasBeenHandled queue)

enumerate :: ((Int, Int), Orientation) -> ((Int, Int), Orientation) -> [((Int, Int), Orientation)]
enumerate ((minX, minY), minDir) ((maxX, maxY), maxDir) = do
  x <- [minX..maxX]
  y <- [minY..maxY]
  dir <- [minDir..maxDir]
  return ((x, y), dir)

data PathInfo = PathInfo {position :: (Int, Int), costSoFar :: Int, expectedFutureCost :: Int, orientation :: Orientation, history :: [(Int, Int)]} deriving (Show)
instance Eq PathInfo where
  (PathInfo posL _ _ dirL _) == (PathInfo posR _ _ dirR _) = (posL == posR) && (dirL == dirR)
instance Ord PathInfo where
  (PathInfo _ ctl cfl _ _) <= (PathInfo _ ctr cfr _ _) = (ctl + cfl) <= (ctr + cfr)

data Dir = U | R | D | L deriving (Eq, Show, Ix, Ord, Enum)
data Orientation = Horizontal | Vertical deriving (Eq, Show, Ix, Ord, Enum)
sidesOf :: Orientation -> [Dir]
sidesOf Vertical = [L, R]
sidesOf Horizontal = [U, D]

perpTo :: Orientation -> Orientation
perpTo Vertical = Horizontal
perpTo Horizontal = Vertical

(+>) :: (Int, Int) -> Dir -> (Int, Int)
(+>) (x, y) U = (x, y-1)
(+>) (x, y) R = (x+1, y)
(+>) (x, y) D = (x, y+1)
(+>) (x, y) L = (x-1, y)

totalCost :: Array (Int, Int) Int -> Array ((Int, Int), Orientation) Bool -> Queue.MinQueue PathInfo -> Either String Int
totalCost _ _ q | Queue.null q = Left "Failed to find destination, queue empty"
totalCost grid _ (getMin -> Just (PathInfo pos c _ _ h)) | pos==snd (bounds grid) = Right c
totalCost grid hasBeenHandled q | any (\(PathInfo pos _ _ dir _) -> hasBeenHandled!(pos, dir)) (getMin q) = totalCost grid hasBeenHandled (deleteMin q)
totalCost grid hasBeenHandled q =
  let
    (pi, newQ) = deleteFindMin q
    PathInfo pos _ _ dir _ = pi
    hasNowBeenHandled = hasBeenHandled // [((pos, dir), True)]
    nextUp = nextOptions grid hasNowBeenHandled pi
  in totalCost grid hasNowBeenHandled (foldr insert newQ nextUp)

estimatedCost :: Array (Int, Int) a -> (Int, Int) -> Int
estimatedCost arr (posX, posY) =
  let (_, (destX, destY)) = bounds arr
  in abs (destX - posX) + abs (destY - posY)

nextOptions :: Array (Int, Int) Int -> Array ((Int, Int), Orientation) Bool -> PathInfo -> [PathInfo]
nextOptions grid hasBeenHandled (PathInfo pos cost _ dir h) =
  let
    advance :: Dir -> ((Int, Int), Int) -> Maybe ((Int, Int), Int)
    advance = \d (p, c) ->
              let p' = p +> d
              in fmap (\c' -> (p', c + c')) (grid?p')
    n = do
        d <- sidesOf dir
        let positions = mutations (advance d) 10 (pos, cost)
        ((p, c), i) <- drop 3 $ zip positions [1..10]
        return (p, c, perpTo dir, h)
  in mapMaybe (toValidInfo grid hasBeenHandled) n

mutations :: (a -> Maybe a) -> Int -> a -> [a]
mutations _ 0 _ = []
mutations f i a = case f a of
  Just a' -> a' : mutations f (i-1) a'
  Nothing -> []

toValidInfo :: Array (Int, Int) Int -> Array ((Int, Int), Orientation) Bool -> ((Int, Int), Int, Orientation, [(Int, Int)]) -> Maybe PathInfo
toValidInfo grid hasBeenHandled (pos, cost, dir, h) = do
  () <- if hasBeenHandled!(pos, dir) then Nothing else Just ()
  return $ PathInfo pos cost (estimatedCost grid pos) dir (pos:h)