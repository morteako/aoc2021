{-# LANGUAGE AllowAmbiguousTypes #-}

module AoC2021.Day09 where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)

index :: [b] -> [(Int, b)]
index = zip [0 ..]

parse :: String -> Map.Map (V2 Int) Int
parse = Map.fromList . concatMap f . index . fmap index . (fmap . fmap) digitToInt . lines
 where
  f (x, xs) = fmap (g x) xs
  g x (y, xs) = (V2 x y, xs)

solveA m = sum $ fmap (+ 1) $ Map.filterWithKey f m
 where
  f k x = all (\n -> Map.findWithDefault 100 n m > x) $ neighs k

neighs :: V2 Int -> [V2 Int]
neighs xy = do
  (x, y) <- zip [1, (-1), 0, 0] [0, 0, 1, (-1)]
  pure $ xy + (V2 x y)

-- findBasin :: V2 Int -> Int -> Map.Map (V2 Int) Int -> _
findBasin :: Ord t => V2 Int -> t -> Map.Map (V2 Int) t -> [(V2 Int, t)]
findBasin curK curV m = (curK, curV) : news <> concatMap (\(k, v) -> findBasin k v m) news
 where
  news = filter (\(_, v) -> v > curV) $ mapMaybe look $ neighs curK
  look k = fmap (k,) $ Map.lookup k m

minKV :: Map.Map a Int -> (a, Int)
minKV = minimumOn snd . Map.toList

findAllBasin :: Map.Map (V2 Int) Int -> [Map.Map (V2 Int) Int]
findAllBasin m | null m = []
findAllBasin m = nm : findAllBasin (Map.difference m nm)
 where
  nm = Map.fromList $ findBasin k v m
  (k, v) = minKV m

solveB :: Map.Map (V2 Int) Int -> Int
solveB xs = product $ take 3 $ reverse $ sort $ fmap length $ findAllBasin removed9s
 where
  removed9s = Map.filter (/= 9) xs

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 506

  let resB = solveB parsed
  print resB
  resB @=? 931200