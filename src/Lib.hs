module Lib
    ( count
    , lastElemIndex
    , anyways
    , isInBounds
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