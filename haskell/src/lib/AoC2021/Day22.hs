module AoC2021.Day22 where

import Data.List.Split
import qualified Data.Map as Map
import Linear
import Test.HUnit ((@=?))
import Utils

parse = fmap (f . words) . lines
 where
  f [toBool -> toggle, splitOn "," -> xyz] = (fmap toRange xyz, toggle)
  f _ = undefined

  toBool "on" = True
  toBool "off" = False
  toBool _ = undefined

  toRange (drop 2 -> splitOn ".." -> fmap readInt -> [lo, up]) = (lo, up)

  comb [xs, ys, zs] = V3 <$> xs <*> ys <*> zs

-- toRange

solveA = Map.size . Map.filter id . foldl1 (Map.unionWith (flip const)) . fmap makeMap
 where
  makeMap (fmap fit -> [xs, ys, zs], toggle) =
    Map.fromList $ fmap (,toggle) (V3 <$> xs <*> ys <*> zs)

  fit (lo, up) = [max (-50) lo .. min 50 up]

-- solveA = Map.size . Map.filter id . foldl1 (Map.unionWith (flip const)) . fmap filterSmall
--  where
--   filterSmall = Map.filterWithKey (\k _ -> Map.member k startMap)
--   startMap = Map.fromList $ fmap (,False) (V3 <$> r <*> r <*> r)
--   r = [-50 .. 50]

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA