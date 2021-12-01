module AoC2021.Day01 where

import Control.Monad (guard, replicateM)

parse :: String -> [Int]
parse = fmap (read @Int) . lines

solveA :: [Int] -> Int
solveA = length . filter (uncurry (<)) . (zip <*> tail)

solveB :: [Int] -> Int
solveB = length . filter (uncurry (<)) . drop 3 . (zip <*> tail) . fmap (sum . take 3) . scanl (flip (:)) []

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solveA parsed
  print $ solveB parsed