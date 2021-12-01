module AoC2021.Day01 where

import Test.HUnit ((@=?))

parse :: String -> [Int]
parse = fmap (read @Int) . lines

countIncreases :: [Int] -> Int
countIncreases = length . filter (uncurry (<)) . (zip <*> tail)

solveA :: [Int] -> Int
solveA = countIncreases

solveB :: [Int] -> Int
solveB = countIncreases . (fmap (sum . take 3) . drop 3 . scanl (flip (:)) [])

slidingTriples :: [a] -> [[a]]
slidingTriples = drop 3 . scanl (flip (:)) []

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  resA @=? 1715
  let resB = solveB parsed
  print resB
  resB @=? 1739
