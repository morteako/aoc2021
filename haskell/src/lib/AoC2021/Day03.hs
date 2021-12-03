module AoC2021.Day03 where

import Data.Char (digitToInt)
import Data.Digits (unDigits)
import Data.List (sortOn, transpose)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Test.HUnit ((@=?))

parse :: String -> [[Int]]
parse = fmap (fmap digitToInt) . lines

occs :: [Int] -> [(Int, Integer)]
occs = sortOn swap . Map.toList . Map.fromListWith (+) . map (,1)

solveA :: [[Int]] -> Int
solveA = product . fmap (unDigits 2) . transpose . fmap (fmap fst . occs) . transpose

match :: ([(Int, Integer)] -> (Int, Integer)) -> Int -> [[Int]] -> [[Int]]
match f i xs = filter (\x -> x !! i == t) xs
 where
  t = fst . f . occs . map (!! i) $ xs

go :: ([(Int, Integer)] -> (Int, Integer)) -> Int -> [[Int]] -> [Int]
go f i [x] = x
go f i xs = go f (i + 1) (match f i xs)

solveB :: [[Int]] -> Int
solveB xs = product . fmap (unDigits 2) $ [go (!! 0) 0 xs, go (!! 1) 0 xs]

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  resA @=? 2498354

  let resB = solveB $ parsed
  print resB
  resB @=? 3277956