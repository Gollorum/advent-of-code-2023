module D06 (execSample, execFinal) where

execSample :: IO ()
execSample = exec "input/sample_06.txt"

execFinal :: IO ()
execFinal = exec "input/06.txt"

exec :: String -> IO ()
exec path = do
  content <- readFile path
  let res = tryExec content
  case res of
    Just str -> print str
    Nothing -> print "Error :("

tryExec :: String -> Maybe String
tryExec content = do
  (times, distances, totalTime, totalDistance) <- case lines content of
    [t, d] -> Just (numsFrom t, numsFrom d, singleNumFrom t, singleNumFrom d)
    _ -> Nothing
  let races = zip times distances
  let numVariants = product (map numVariantsFor races)
  let part2Variants = numVariantsFor (totalTime, totalDistance)
  Just (show numVariants ++ " | " ++ show part2Variants)

numsFrom :: String -> [Float]
numsFrom l = map read (tail (words l))

singleNumFrom :: String -> Float
singleNumFrom l = read (filter (/= ' ') (drop 10 l))

-- d = t * (T-t)
--   = tT - t²
-- 0 = t² - tT + d
-- t = T/2 +- sqrt((T/2)² - d)

lowerBoundFor :: (Float, Float) -> Int
lowerBoundFor (t, d) = ceiling (t / 2 - sqrt (t*t/4 - d - 1))

upperBoundFor :: (Float, Float) -> Int
upperBoundFor (t, d) = floor (t / 2 + sqrt (t*t/4 - d - 1))

numVariantsFor :: (Float, Float) -> Int
numVariantsFor race = upperBoundFor race - lowerBoundFor race + 1