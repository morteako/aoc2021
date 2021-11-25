module AoC2020.Day02 where

import Data.Bits (Bits (xor))
import Data.List.Extra (splitOn)
import Safe (atMay)

data Line = Line Int Int Char String deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

parse :: String -> [Line]
parse = fmap (toLine . words) . lines
 where
  toLine [splitOn "-" -> [a, b], [c, ':'], str] = Line (read a) (read b) c str
  toLine xs = error $ show xs

validatePart1 :: Line -> Bool
validatePart1 (Line a b c str) = a <= count (== c) str && b >= count (== c) str

validatePart2 :: Line -> Bool
validatePart2 (Line (pred -> a) (pred -> b) c str) = (Just c == atMay str a) `xor` (Just c == atMay str b)

solve :: (a -> Bool) -> [a] -> Int
solve = count

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve validatePart1 parsed --550
  print $ solve validatePart2 parsed --634
