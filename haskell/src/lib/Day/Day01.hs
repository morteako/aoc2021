module Day.Day01 where

import Control.Lens
import Control.Monad (guard, replicateM)
import Data.Bitraversable (Bitraversable (bitraverse))

solve :: (Eq a, Num a) => Int -> [a] -> a
solve n nums = head $ do
  xs <- replicateM n nums
  guard (sum xs == 2020)
  pure $ product xs

parse :: String -> [Int]
parse = fmap read . lines

run :: String -> IO (String, String)
run xs = do
  let parsed = parse xs
  let a = solve 2 parsed
  let b = solve 3 parsed
  return (show a, show b)