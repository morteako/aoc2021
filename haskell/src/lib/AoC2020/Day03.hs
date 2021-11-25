module AoC2020.Day03 where

import Control.Monad ((>=>))
import Data.List (unfoldr)
import Safe (headMay)
import Utils (count)

walk :: Int -> Int -> [[a]] -> [a]
walk right down = unfoldr move
 where
  move [] = Nothing
  move xs = do
    let movedRight = fmap (drop right) xs
    let movedDown = drop down movedRight
    a <- (headMay >=> headMay) movedDown
    pure (a, movedDown)

walkWith :: Int -> Int -> [String] -> Int
walkWith right down = count '#' . walk right down

solve1 :: [String] -> Int
solve1 = walkWith 3 1

solve2 :: [String] -> Int
solve2 xs = product [f 1 1, f 3 1, f 5 1, f 7 1, f 1 2]
 where
  f n m = walkWith n m xs

parse :: String -> [String]
parse = fmap cycle . lines

run :: String -> IO ()
run (parse -> parsed) = do
  print $ solve1 parsed -- 225
  print $ solve2 parsed -- 1115775000
