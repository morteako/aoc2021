{-# LANGUAGE FlexibleContexts #-}

module AoC2021.Day20 where

import AoC2021.Day17 (toTup)
import Control.Arrow
import Control.Lens
import Data.Digits (unDigits)
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Linear

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map.Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold (V2 Int) String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

fromChar :: Char -> Maybe Int
fromChar '#' = Just 1
fromChar '.' = Just 0
fromChar _ = Nothing

neighs :: V2 Int -> [V2 Int]
neighs xy = do
  x <- [1, 0, (-1)]
  y <- [1, 0, (-1)]
  -- guard (x /= 0 || y /= 0)
  pure $ V2 x y + xy

lookupGridNum :: Map.Map Int b -> Map.Map (V2 Int) Int -> V2 Int -> b
lookupGridNum bitmap grid v =
  v
    & neighs
    & fmap (\x -> Map.findWithDefault 0 x grid)
    & unDigits 2
    & (bitmap Map.!)

parse :: [Char] -> (Map.Map Int Int, Map.Map (V2 Int) Int)
parse = (f *** parseAsciiMap fromChar) . toTup . splitOn "\n\n"
 where
  f = Map.fromList . zip [0 ..] . mapMaybe fromChar

solveA (bitMap, grid) = enhance $ enhance grid
 where
  enhance g = Map.mapWithKey (\k _ -> lookupGridNum bitMap g k) g

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  -- mapM print $ Map.toList resA
  print resA
