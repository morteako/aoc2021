{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}

module AoC2021.Day08 where

import Control.Arrow
import Data.Foldable

import Data.List (permutations, sort)
import Data.List.Extra (groupOn, sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit ((@=?))

solveA :: [(a, [String])] -> Sum Int
solveA = foldMap (Sum . length . filter (\x -> Map.member (length x) uniqLengthss) . snd)
 where
  uniqLengthss = Map.filter (\x -> length x == 1) lendigits

parse :: String -> [([String], [String])]
parse = fmap (findNumber . fmap words . splitOn "|") . lines
 where
  findNumber [x, y] = (x, y)
  findNumber xs = error $ show xs

digs :: [String]
digs = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

indDigs :: Map String Char
indDigs = Map.fromList $ flip zip ['0' ..] digs

unionsIntersect :: [Map Char (Set Char)] -> Map Char (Set Char)
unionsIntersect = Map.unionsWith (Set.intersection)

lendigits :: Map Int [Set Char]
lendigits =
  Map.fromListWith (++) $ fmap (length &&& (: []) . Set.fromList) digs

solveB :: [([String], [String])] -> Int
solveB = sum . map findNumber

findNumber :: ([String], [String]) -> Int
findNumber (encodedDigits, digs) = combineNumbers $ fmap decode digs
 where
  combineNumbers = read . map (indDigs Map.!)
  decode = sort . fmap (digitMapping Map.!)
  groupLength = groupOn length . sortOn length
  digitMapping =
    head
      . fmap (Map.fromList . fmap (fmap Set.findMin))
      . mapMaybe topsort
      . combineMappings
      . groupLength
      $ encodedDigits

combineMappings :: [[String]] -> [Map Char (Set Char)]
combineMappings = fmap unionsIntersect . traverse possibleMappings
 where
  possibleMappings :: [String] -> [Map Char (Set Char)]
  possibleMappings strGroup = fmap unionsIntersect $ do
    p <- permutations strGroup
    pure $ do
      str <- strGroup
      zipWith (\cur target -> Map.fromList $ fmap (\c -> (c, target)) cur) p (lendigits Map.! length (head strGroup))

topsort :: Map Char (Set Char) -> Maybe [(Char, Set Char)]
topsort xs
  | Map.null xs = Just []
  | Map.null ones = Nothing
  | otherwise = fmap (os ++) $ topsort $ foldl' (\acc o -> fmap (flip Set.difference o) acc) rest $ fmap snd os
 where
  (ones, rest) = Map.partition (\x -> length x == 1) xs
  os = Map.toList ones

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 303

  let resB = solveB parsed
  print resB
  resB @=? 961734
