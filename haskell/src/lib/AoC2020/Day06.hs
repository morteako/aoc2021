module AoC2020.Day06 where

import Data.List.Split (splitOn)
import Data.Monoid
import qualified Data.Set as Set

parse :: String -> [[String]]
parse = fmap lines . splitOn "\n\n"

solve f = foldMap (Sum . Set.size . foldr1 f . fmap Set.fromList)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve Set.union parsed -- 6596
  print $ solve Set.intersection parsed -- 3219
