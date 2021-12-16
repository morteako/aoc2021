{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Strict #-}

module AoC2021.Day15 where

import Control.Lens hiding (index)
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Graph.Inductive (Graph (mkGraph), sp, spLength)
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)

index :: [b] -> [(Int, b)]
index = zip [0 ..]

downRight :: V2 Int -> [V2 Int]
downRight xy = do
  fmap (+ xy) [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

parse :: String -> Map.Map (V2 Int) Int
parse = Map.fromList . concatMap f . index . fmap index . (fmap . fmap) digitToInt . lines
 where
  f (x, xs) = fmap (g x) xs
  g x (y, xs) = (V2 x y, xs)

incMod10 9 = 1
incMod10 n = n + 1

times 0 f x = x
times n f x = times (n -1) f (f x)

makeGrid5 m = Map.unions $ fmap f factors
 where
  f fact@(V2 x y) = fmap (times (x + y) incMod10) $ Map.mapKeys (\k -> k + (lim * fact)) m
  lim = (+ 1) $ fst $ Map.findMax m
  factors = do
    x <- [0 .. 4]
    y <- [0 .. 4]
    [V2 x y]

solve :: (t -> Map.Map (V2 Int) Int) -> t -> Maybe Int
solve f grid = spLength start dest gr
 where
  (gr, start, dest) = makeGraph $ f grid

makeGraph :: Map.Map (V2 Int) Int -> (Gr (V2 Int) Int, Int, Int)
makeGraph grid = (mkGraph nodes edges, 0, Set.size indexes - 1)
 where
  nodes = zip [0 :: Int ..] $ Set.toList indexes
  toIndex (a, b, c) = (Set.findIndex a indexes, Set.findIndex b indexes, c)
  f k = mapMaybe (\x -> (k,x,) <$> Map.lookup x grid) $ downRight k
  edges = fmap toIndex $ Map.foldMapWithKey (\k _ -> f k) grid
  indexes = Map.keysSet grid

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solve id parsed
  print resA
  resA @=? Just 429

  let resB = solve makeGrid5 parsed
  print resB
  resB @=? Just 2844