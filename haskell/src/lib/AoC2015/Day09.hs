module AoC2015.Day09 where

import Control.Applicative
import Control.Lens
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Test.HUnit ((@=?))

parse :: String -> Map [Char] (Map [Char] Int)
parse = Map.fromListWith (<>) . concatMap (f . words) . lines
 where
  f [x, "to", y, "=", i] = [(x, Map.singleton y (read @Int i)), (y, Map.singleton x (read @Int i))]
  f xs = error $ show xs

solve :: (forall f. Foldable f => f Int -> Int) -> Map [Char] (Map [Char] Int) -> Int
solve f m = f $ Map.mapMaybeWithKey (\k _ -> findPath f k m) m

findPath :: ([Int] -> Int) -> String -> Map [Char] (Map [Char] Int) -> Maybe Int
findPath f target m | Map.size m == 1 && Map.member target m = Just 0
findPath f target m =
  case Map.lookup target m of
    Nothing -> Nothing
    Just mm ->
      case catMaybes . fmap (\(k, v) -> (+ v) <$> findPath f k (Map.delete target m)) $ Map.toList mm of
        [] -> Nothing
        xs -> Just $ f xs

run :: String -> IO (String, String)
run xs = do
  let parsed = parse xs
  let resA = solve minimum parsed
  print resA
  resA @=? 141
  let resB = solve maximum parsed
  print resB
  resB @=? 736

  return mempty

-- i : 1.36