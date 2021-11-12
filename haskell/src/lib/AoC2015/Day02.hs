module AoC2015.Day02 where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Semigroup (Sum (Sum))

data Lwh = Lwh Int Int Int deriving (Show)

solveA :: [Lwh] -> Sum Int
solveA = foldMap (Sum . calcWrapping)

calcWrapping :: Lwh -> Int
calcWrapping (Lwh l w h) = 2 * sum sides + minimum sides
 where
  sides = [l * w, w * h, h * l]

solveB :: [Lwh] -> Sum Int
solveB = foldMap (Sum . calcRibbon)

calcRibbon :: Lwh -> Int
calcRibbon (Lwh l w h) = 2 * sum (take 2 sortedDims) + product sortedDims
 where
  sortedDims = sort [l, w, h]

parse :: String -> [Lwh]
parse = fmap (f . splitOn "x") . lines
 where
  f (fmap read -> [l, w, h]) = Lwh l w h
  f xs = error $ show xs

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let a = solveA parsed
  let b = solveB parsed
  print a
  print b
