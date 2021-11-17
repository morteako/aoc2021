module AoC2015.Day17 where

import Data.List (genericLength)
import Data.Semigroup (Min)
import Test.HUnit ((@=?))

parse :: String -> [Int]
parse = fmap (read @Int) . lines

findCombs :: Int -> [Int] -> [[Int]]
findCombs = go
 where
  go n _ | n < 0 = []
  go 0 _ = [[]]
  go n [] = []
  go n (x : xs) = map (x :) (go (n - x) xs) ++ go n xs

solveA :: [Int] -> Int
solveA = length . findCombs 150

solveB :: [Int] -> Int
solveB = length . findShortestLists . findCombs 150

findShortestLists :: [[a]] -> [[a]]
findShortestLists = fst . foldr f ([], mempty)
 where
  f a (is, curMin :: Min Int) = case compare (genericLength a) curMin of
    GT -> (is, curMin)
    EQ -> (a : is, curMin)
    LT -> ([a], genericLength a)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  resA @=? 1304

  let resB = solveB parsed
  print resB
  resB @=? 18

-- c: 0.00s
-- i: 0.34s