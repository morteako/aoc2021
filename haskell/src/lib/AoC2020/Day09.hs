{-# LANGUAGE BangPatterns #-}

module AoC2020.Day09 where

import Data.Coerce
import qualified Data.IntSet as Set
import Data.List
import Data.Semigroup

parse :: Int -> String -> ([Int], [Int])
parse pre = splitAt pre . fmap (read @Int) . lines

check _ _ [] = False
check prev target (x : xs) = Set.member x prev || check (Set.insert (target - x) prev) target xs

solve1 (pre, xs) = f pre xs
  where
    f pre (x : xs)
      | check mempty x pre = f (tail pre ++ [x]) xs
      | otherwise = x

solve2 :: ([Int], [Int]) -> [Int]
solve2 (pre, xs) = map calc $ filter isInv $ coerce $ concatMap (scanl1 (<>)) $ tails $ map toMon (pre ++ xs)
  where
    toMon x = (Min x, Max x, Sum x)
    isInv (_, _, x) = x == invalidNum
    invalidNum = solve1 (pre, xs)

    calc (ma, mi, _) = ma + mi

run xs = do
  let parsed = parse 25 xs

  print $ solve1 parsed
  print $ solve2 parsed
