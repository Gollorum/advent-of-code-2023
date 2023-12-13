module Lib
    ( count
    , lastElemIndex
    ) where


count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (h : t) = if f h then 1 + count f t else count f t

lastElemIndex :: Char -> [Char] -> Maybe Int
lastElemIndex _ [] = Nothing
lastElemIndex c (h : t) = case lastElemIndex c t of
  Just i -> Just (i + 1)
  Nothing -> if c == h then Just 0 else Nothing
