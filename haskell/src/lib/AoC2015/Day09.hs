{-# LANGUAGE FlexibleContexts #-}

module AoC2015.Day09 where

import Control.Lens
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Test.HUnit ((@=?))

parse :: String -> Map [Char] (Map [Char] Int)
parse = Map.fromListWith (<>) . concatMap (f . words) . lines
 where
  f [x, "to", y, "=", i] = [(x, Map.singleton y (read @Int i)), (y, Map.singleton x (read @Int i))]
  f xs = error $ show xs


solve f m = fromJust $ f folded $ Map.mapMaybeWithKey (\k _ -> findPath k m) m
 where
  findPath :: String -> Map [Char] (Map [Char] Int) -> Maybe Int
  findPath target m | Map.size m == 1 && Map.member target m = Just 0
  findPath target m =
    case Map.lookup target m of
      Nothing -> Nothing
      Just mm -> f (ifolded . withIndex . to (\(k, v) -> (+ v) <$> findPath k (Map.delete target m)) . _Just) mm

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solve minimumOf parsed
  print resA
  resA @=? 141
  let resB = solve maximumOf parsed
  print resB
  resB @=? 736

-- i : 1.7s