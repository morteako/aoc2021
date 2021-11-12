module AoC2015.Day05 where

import Data.List (group, inits, isInfixOf, tails)
import Data.List.Extra (dropEnd, takeEnd)

parse :: String -> [String]
parse = lines

solveA :: [String] -> Int
solveA = length . filter (\x -> threeVowels x && twos x && notContains x)
 where
  threeVowels = (>= 3) . length . filter (\x -> elem @[] x "aeiou")
  twos = any ((>= 2) . length) . group
  notContains q = not $ any (`isInfixOf` q) ["ab", "cd", "pq", "xy"]

paps :: [a] -> [([a], [a])]
paps = drop 2 . (zip <$> fmap (takeEnd 2) . inits <*> tails)

solveB :: [String] -> Int
solveB = length . filter (\x -> twice x && palin x)
 where
  twice = any (uncurry isInfixOf) . paps
  palin = any ((==) <*> reverse) . fmap (take 3) . dropEnd 3 . tails

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let a = solveA parsed
  let b = solveB parsed
  print a
  print b

-- 258
-- 53