module AoC2020.Day19 where

import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.IntMap.Strict as Map
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as RP

data Rule = Single String | Conc [Int] | Many [Rule] deriving (Show)

parse :: [Char] -> (Map.IntMap Rule, [String])
parse xs = case fmap lines $ splitOn "\n\n" xs of
  [map f -> rules, msgs] -> (Map.fromList $ rules, msgs)
  where
    f (splitOn ": " -> [n, rule]) = (read @Int n, readRule rule)

    readRule str@('"' : _) = Single $ read str
    readRule (splitOn " | " -> rules) = case rules of
      [xs] -> Conc $ map read $ words xs
      _ -> Many $ map readRule $ rules

parserFromRule :: Map.IntMap Rule -> Rule -> RP.ReadP ()
parserFromRule m (Single s) = RP.string s $> ()
parserFromRule m (Conc xs) = traverse_ (parserFromRule m . (m Map.!)) xs
parserFromRule m (Many xs) = RP.choice (map (parserFromRule m) xs)

countParses :: Map.IntMap Rule -> [String] -> Int
countParses ruleMap msgs = length $ filter (not . null) $ map findParses msgs
  where
    findParses s = filter (null . snd) $ RP.readP_to_S parser s
    parser = parserFromRule ruleMap (ruleMap Map.! 0)

solve1 :: (Map.IntMap Rule, [String]) -> Int
solve1 = uncurry countParses

solve2 :: (Map.IntMap Rule, [String]) -> Int
solve2 (fixRuleMap -> ruleMap, msgs) = countParses ruleMap msgs

fixRuleMap :: Map.IntMap Rule -> Map.IntMap Rule
fixRuleMap = Map.insert 8 (Many [Conc [42], Conc [42, 8]]) . Map.insert 11 (Many [Conc [42, 31], Conc [42, 11, 31]])

run :: [Char] -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve1 $ parsed -- 174
  print $ solve2 $ parsed -- 367
