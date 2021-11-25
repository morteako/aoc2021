module AoC2020.Day15 where

import qualified Data.IntMap.Strict as Map
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn "," . head . lines

solve :: Int -> [Int] -> Int
solve lim xs = go (Map.fromList $ zip xs [1 ..]) (last xs) (length xs + 1)
 where
  go cur prev i
    | i > lim = prev
    | let subi1 x = i - 1 - x
      , let (l, newCur) = Map.insertLookupWithKey (\_ a _ -> a) prev (i - 1) cur =
      go newCur (maybe 0 subi1 l) (i + 1)

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  print $ solve 2020 parsed -- 436
  print $ solve 30000000 parsed -- 116590
