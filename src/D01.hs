{-# LANGUAGE ViewPatterns #-}
module D01 (execSample, execFinal) where

import Data.Char (isDigit, digitToInt)
import Data.List (stripPrefix)

execSample :: IO ()
execSample = exec "input/sample_01.txt"

execFinal :: IO ()
execFinal = exec "input/01.txt"

fetchDigits :: String -> [Int]
fetchDigits (stripPrefix "one" -> Just rest) = 1 : fetchDigits ("e" ++ rest)
fetchDigits (stripPrefix "two" -> Just rest) = 2 : fetchDigits ("o" ++ rest)
fetchDigits (stripPrefix "three" -> Just rest) = 3 : fetchDigits ("e" ++ rest)
fetchDigits (stripPrefix "four" -> Just rest) = 4 : fetchDigits ("r" ++ rest)
fetchDigits (stripPrefix "five" -> Just rest) = 5 : fetchDigits ("e" ++ rest)
fetchDigits (stripPrefix "six" -> Just rest) = 6 : fetchDigits ("x" ++ rest)
fetchDigits (stripPrefix "seven" -> Just rest) = 7 : fetchDigits ("n" ++ rest)
fetchDigits (stripPrefix "eight" -> Just rest) = 8 : fetchDigits ("t" ++ rest)
fetchDigits (stripPrefix "nine" -> Just rest) = 9 : fetchDigits ("e" ++ rest)
fetchDigits (prefix:rest) = if isDigit prefix
  then digitToInt prefix : fetchDigits rest
  else fetchDigits rest
fetchDigits "" = []

digitsToNum :: [Int] -> Int
digitsToNum digits = head digits * 10 + last digits

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let ls = lines content
  let nums = [(digitsToNum . fetchDigits) x | x <- ls]
  print (sum nums)