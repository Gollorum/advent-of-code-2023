module D07 (execSample, execFinal) where

import Data.Int (Int64)
import Data.Char (isDigit, digitToInt)
import Data.List
import Data.Ord

execSample :: IO ()
execSample = exec "input/sample_07.txt"

execFinal :: IO ()
execFinal = exec "input/07.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let deck = mapM handFrom ls
  let sorted = fmap (\d -> zip (sort d) [1..]) deck
  case sorted of
    Just d -> print (sum (map (\(c, i) -> i * bid c) d))
    Nothing -> print ":("

newtype Card = Card {value :: Int}
instance Eq Card where
  c == c' = value c == value c'
instance Ord Card where
  c `compare` c' = value c `compare` value c'
instance Show Card where
  show c = show (value c)

parse :: Char -> Maybe Card
parse 'A' = Just (Card 14)
parse 'K' = Just (Card 13)
parse 'Q' = Just (Card 12)
parse 'J' = Just (Card 1)
parse 'T' = Just (Card 10)
parse d = if isDigit d
  then Just (Card (digitToInt d))
  else Nothing

data Hand = Hand {cards :: [Card], bid :: Int64, primaryStrength :: Int, secondaryStrength :: Int}
instance Show Hand where
  show (Hand c b ps ss) = "Cards: " ++ show c ++ " | bid: " ++ show b ++ " | ps: " ++ show ps ++ " | ss: " ++ show ss
instance Eq Hand where
  (Hand _ _ ps1 ss1) == (Hand _ _ ps2 ss2) = (ps1 == ps2) && (ss1 == ss2)
instance Ord Hand where
  (Hand _ _ ps1 ss1) `compare` (Hand _ _ ps2 ss2) = if ps1 == ps2 then compare ss1 ss2 else compare ps1 ps2

handFrom :: String -> Maybe Hand
handFrom l = do
  (cStr, b) <- case l of
    c1 : c2 : c3 : c4 : c5 : ' ' : bidStr -> Just ([c1, c2, c3, c4, c5], read bidStr :: Int64)
    _ -> Nothing
  c <- mapM parse cStr
  let mostAbundant = (if c == [Card 1, Card 1, Card 1, Card 1, Card 1] then Card 14 else snd (minimumBy (\l r -> compare (fst r) (fst l)) ((map (\g -> (length g, head g)) . group . sort) (filter (\g -> value g /= 1) c))))
  let cardsWithoutJoker = [if value card == 1 then mostAbundant else card | card <- c]
  let occurences = sortBy (flip compare) ((map length . group . sort) cardsWithoutJoker) :: [Int]
  let srtPrim = primaryStrengthOf occurences
  let strSec = foldl (\s cur -> s * 15 + value cur) 0 c
  Just (Hand c b srtPrim strSec)

primaryStrengthOf :: [Int] -> Int
primaryStrengthOf [5] = 7
primaryStrengthOf [4, 1] = 6
primaryStrengthOf [3, 2] = 5
primaryStrengthOf [3, 1, 1] = 4
primaryStrengthOf [2, 2, 1] = 3
primaryStrengthOf (2 : _) = 2
primaryStrengthOf _ = 1