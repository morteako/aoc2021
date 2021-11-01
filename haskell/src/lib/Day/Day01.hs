module Day.Day01 where

import Control.Monad (guard, replicateM)

solve :: (Eq a, Num a) => Int -> [a] -> a
solve n nums = head $ do
  xs <- replicateM n nums
  guard (sum xs == 2020)
  pure $ product xs

parse :: String -> [Int]
parse = fmap read . lines

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve 2 parsed
  print $ solve 3 parsed