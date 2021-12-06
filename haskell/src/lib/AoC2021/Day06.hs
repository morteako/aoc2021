module AoC2021.Day06 where

import qualified Data.IntMap.Strict as Map
import Data.List.Extra (splitOn)
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: [Char] -> [Int]
parse = fmap readInt . splitOn ","

qq :: Map.IntMap Int -> Map.IntMap Int
qq xs
  | let m = Map.mapKeys (subtract 1) xs = case Map.lookup (-1) m of
    Nothing -> m
    Just q -> Map.insertWith (+) 6 q $ Map.insertWith (+) 8 q $ Map.delete (-1) m

solve :: Int -> [Int] -> Int
solve n = sum . (!! n) . iterate qq . Map.fromListWith (+) . fmap (,1)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solve 80 parsed
  print resA
  resA @=? 360610

  let resB = solve 256 parsed
  print resB
  resB @=? 1631629590423