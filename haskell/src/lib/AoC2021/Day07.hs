module AoC2021.Day07 where

import Data.List.Extra (minimumOn, splitOn, sumOn')
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: [Char] -> [Int]
parse = fmap readInt . splitOn ","

triangle :: Integral a => a -> a
triangle n = (n * (n + 1)) `div` 2

solve :: (Int -> Int) -> [Int] -> Int
solve f xs = minimum $ do
  t <- [0 .. maximum xs]
  pure $ sumOn' (\x -> f $ abs $ x - t) xs

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solve id parsed
  print resA
  resA @=? 344605

  let resB = solve triangle parsed
  print resB
  resB @=? 93699985