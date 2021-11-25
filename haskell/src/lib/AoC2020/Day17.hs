{-# LANGUAGE FlexibleContexts #-}

module AoC2020.Day17 where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Linear

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold (V2 Int) String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

fromChar :: Char -> Maybe Bool
fromChar '#' = Just True
fromChar '.' = Just False
fromChar _ = Nothing

parse :: String -> Map (V3 Int) Bool
parse s = m <> falses
 where
  falses = Map.fromList $ map (,False) (V3 <$> [-15 .. 15] <*> [-15 .. 15] <*> [-15 .. 15])
  m = Map.mapKeys toV3 . parseAsciiMap fromChar $ s

  toV3 (V2 x y) = (V3 x y 0)

parse2 :: String -> Map (V4 Int) Bool
parse2 s = m <> falses
 where
  falses = Map.fromList $ map (,False) (V4 <$> [-15 .. 15] <*> [-15 .. 15] <*> [-15 .. 15] <*> [-15 .. 15])
  m = Map.mapKeys toV4 . parseAsciiMap fromChar $ s
  toV4 (V2 x y) = (V4 x y 0 0)

nbs :: V3 Int -> [V3 Int]
nbs (v3 :: V3 Int) = tail $ do
  fx <- [id, succ, pred]
  fy <- [id, succ, pred]
  fz <- [id, succ, pred]
  pure $ over _x fx $ over _y fy $ over _z fz v3

nbs4 :: V4 Int -> [V4 Int]
nbs4 (v4 :: V4 Int) = tail $ do
  fx <- [id, succ, pred]
  fy <- [id, succ, pred]
  fz <- [id, succ, pred]
  fw <- [id, succ, pred]
  pure $ over _x fx $ over _y fy $ over _z fz $ over _w fw v4

rundayGen getNbs c grid = if c == 6 then grid else rundayGen getNbs (c + 1) newGrid
 where
  addedGrid = grid
  newGrid = Map.mapWithKey f addedGrid
  f point seat = case seat of
    True -> occNbs == 2 || occNbs == 3
    False -> occNbs == 3
   where
    occNbs = length $ take 4 $ filter id $ mapMaybe (flip Map.lookup addedGrid) (getNbs point)

solve1 :: Map (V3 Int) Bool -> Int
solve1 m = Map.size . Map.filter id . rundayGen nbs 0 $ m

solve2 :: Map (V4 Int) Bool -> Int
solve2 m = Map.size . Map.filter id . rundayGen nbs4 0 $ m

run xs = do
  print $ solve1 $ parse xs
  print $ solve2 $ parse2 xs