{-# LANGUAGE TemplateHaskell #-}

module AoC2021.Day10 where

import Control.Lens
import Data.List (foldl', sort)
import qualified Data.Map as Map
import Data.Semigroup
import Test.HUnit ((@=?))

parse :: String -> [String]
parse = lines

leftToRight :: Char -> Maybe Char
leftToRight = flip Map.lookup (Map.fromList $ zip "({[<" ")}]>")

data Result = Ok | Incomplete String | Corrupted Char deriving (Show)

makePrisms ''Result

check :: String -> String -> Result
check "" "" = Ok
check (p : ps) "" = Incomplete (p : ps)
check ps (c : cs)
  | Just r <- leftToRight c = check (r : ps) cs
check (p : ps) (c : cs) | p == c = check ps cs
check _ (c : cs) = Corrupted c

solveA :: [String] -> Integer
solveA = sumOf (each . to (check "") . _Corrupted . to score)
 where
  score x = case x of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> error $ show x

solveB :: [String] -> Integer
solveB = getMiddle . toListOf (each . to (check "") . _Incomplete . to scoreIncles)
 where
  scoreIncles = foldl' (\a b -> score b + 5 * a) 0

  score x = case x of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _ -> error $ show x

getMiddle :: Ord a => [a] -> a
getMiddle (sort -> xs) = xs !! (length xs `div` 2)

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 339477

  let resB = solveB parsed
  print resB
  resB @=? 3049320156
