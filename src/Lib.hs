module Lib
    ( count
    , lastElemIndex
    , anyways
    , isInBounds
    , arrayFrom
    , show2dArray
    , show2dArrayWith
    , show2dArrayWithIdx
    , (?)
    , concatMapM
    , Pos (..)
    ) where

import Data.Array

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (h : t) = if f h then 1 + count f t else count f t

lastElemIndex :: Char -> [Char] -> Maybe Int
lastElemIndex _ [] = Nothing
lastElemIndex c (h : t) = case lastElemIndex c t of
  Just i -> Just (i + 1)
  Nothing -> if c == h then Just 0 else Nothing

anyways :: (a -> b) -> Either a a -> b
anyways f e = case e of
  Left a -> f a
  Right a -> f a
  

data Pos = Pos Int Int deriving (Show, Eq, Ord, Ix)
  
isInBounds :: Pos -> Array Pos a -> Bool
isInBounds (Pos x y) arr =
  let
    ((Pos minX minY), (Pos maxX maxY)) = bounds arr
  in minX <= x && minY <= y && maxX >= x && maxY >= y
  
arrayFrom :: [[a]] -> Array Pos a
arrayFrom rows =
  let
    height = length rows
    width = length $ head rows
    elementsWithIndex = do
                      (y, row) <- zip [0..] rows
                      (x, e) <- zip [0..] row
                      return (Pos x y, e)
  in array (Pos 0 0, Pos (width-1) (height-1)) elementsWithIndex

show2dArray :: Show a => Array Pos a -> String
show2dArray = show2dArrayWith show

show2dArrayWith :: (a -> String) -> Array Pos a -> String
show2dArrayWith shw arr =
  let
    (Pos minX minY, Pos maxX maxY) = bounds arr
  in foldl (\res y -> res ++ foldl (\l x -> l ++ shw (arr!Pos x y)) "" [minY..maxY] ++ "\n") "" [minX..maxX]

show2dArrayWithIdx :: (Pos -> a -> String) -> Array Pos a -> String
show2dArrayWithIdx shw arr =
  let
    (Pos minX minY, Pos maxX maxY) = bounds arr
  in foldl (\res y -> res ++ foldl (\l x -> l ++ shw (Pos x y) (arr!Pos x y)) "" [minY..maxY] ++ "\n") "" [minX..maxX]

(?) :: Ix i => Array i a -> i -> Maybe a
(?) arr i = if inRange (bounds arr) i then Just (arr!i) else Nothing

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f a = concat <$> mapM f a