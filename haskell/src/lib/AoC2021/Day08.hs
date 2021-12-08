module AoC2021.Day08 where

import Data.List.Extra
import qualified Data.Map as Map
import Data.Semigroup
import qualified Data.Set as Set
import Debug.Trace

parse = fmap (f . fmap words . splitOn "|") . lines
 where
  f [x, y] = (x, y)
  f xs = error $ show xs

digits =
  Map.fromList $ zip [0 .. 9] $ fmap Set.fromList $ ["abcefg" :: String, "cf", "acdeg", "abdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

solveA xs = foldMap (Sum . length . id . filter (\x -> length x `elem` uniqLengths) . fmap id . snd) xs
 where

uniqLengths = fmap (length . (digits Map.!)) [1, 4, 7, 8]

poss :: String -> Map.Map Char String
poss s = Map.fromAscList $ foldMap (\x -> [(x, s)]) s

figureDec xs = Map.unionsWith intersect $ fmap poss xs

solveB = Map.toList . head . fmap (figureDec . snd)

run :: String -> IO ()
run xs = do
  let xs = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"

  let parsed = parse xs
  print parsed

  let resA = solveA parsed
  print resA

  print $ poss "abc"

  let resB = solveB parsed
  print resB

-- print $ Map.fromList [('f', "fcadb"), ('c', "cdfb"), ('a', "fcadb"), ('d', "cdbaf"), ('b', "cdbaf"), ('a', "cdbaf"), ('d', "cdfb"), ('b', "fcadb"), ('f', "cdfeb"), ('e', "cdfeb"), ('b', "cdfeb"), ('f', "cdbaf")]

-- print $ fmap length digits
