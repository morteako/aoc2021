module AoC2020.Day10 where

import Data.Function.Memoize (memoize2)
import Data.List (sort)

parse = (0 :) . sort . fmap (read @Int) . lines

solve1 :: (Eq a, Num a) => [a] -> Int
solve1 (zip <*> tail -> xs) = count 1 * (count 3 + 1)
  where
    count n = length $ filter (diffEq n) xs
    diffEq n (x, y) = n == y - x

memCom :: Int -> [Int] -> Integer
memCom = memoize2 com
  where
    com x [y] = if x + 3 >= y then 1 else 0
    com x (y : xs)
      | x + 3 >= y = memCom x xs + memCom y xs
      | otherwise = 0

solve2 :: [Int] -> Integer
solve2 (x : xs) = memCom x xs

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve1 parsed -- 2232
  print $ solve2 parsed -- 173625106649344
