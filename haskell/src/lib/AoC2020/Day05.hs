module AoC2020.Day05 where

import Control.Lens
import Data.List (sort)
import Data.List.GroupBy (groupBy)

data Row = B | F deriving (Show, Read)

data Seat = R | L deriving (Show, Read)

type RowsAndSeat = ([Row], [Seat])

parse :: String -> [RowsAndSeat]
parse = fmap (bimap read' read' . splitAt 7) . lines
 where
  read' :: Read a => String -> [a]
  read' = fmap (read . (: []))

find :: RowsAndSeat -> (Integer, Integer)
find (rows, seats) = (comb fromRow rows (0, 127), comb fromSeat seats (0, 7))
 where
  comb f xs = fst . foldr ((.) . f) id (reverse xs)

  fromRow B = toUpper
  fromRow F = toLower

  fromSeat R = toUpper
  fromSeat L = toLower

  toLower (x, y) = (x, pred y - (y - x) `div` 2)

  toUpper (x, y) = (succ x + (y - x) `div` 2, y)

findIds :: [RowsAndSeat] -> [Integer]
findIds = map (getId . find)
 where
  getId (x, y) = x * 8 + y

solve1 :: [RowsAndSeat] -> Integer
solve1 = maximum . findIds

solve2 :: [RowsAndSeat] -> Integer
solve2 xs = pred . head . head . tail . groupBy ((==) . succ) $ sortedIds
 where
  sortedIds = sort $ findIds xs

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve1 parsed -- 959
  print $ solve2 parsed -- 527