module Lib
    ( count
    , lastElemIndex
    , anyways
    , isInBounds
    , arrayFrom
    , show2dArray
    , (?)
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
  
isInBounds :: (Int, Int) -> (Array (Int, Int) a) -> Bool
isInBounds (x, y) arr =
  let
    ((minX, minY), (maxX, maxY)) = bounds arr
  in minX <= x && minY <= y && maxX >= x && maxY >= y
  
arrayFrom :: [[a]] -> Array (Int, Int) a
arrayFrom rows =
  let
    height = length rows
    width = length $ head rows
    elementsWithIndex = do
                      (y, row) <- zip [0..] rows
                      (x, e) <- zip [0..] row
                      return ((x, y), e)
  in array ((0, 0), (width-1, height-1)) elementsWithIndex

show2dArray :: Show a => Array (Int, Int) a -> String
show2dArray arr =
  let
    ((minX, minY), (maxX, maxY)) = bounds arr
  in foldl (\res y -> res ++ foldl (\l x -> l ++ show (arr!(x, y))) "" [minY..maxY] ++ "\n") "" [minX..maxX]

(?) :: Ix i => Array i a -> i -> Maybe a
(?) arr i = if inRange (bounds arr) i then Just (arr!i) else Nothing