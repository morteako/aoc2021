{-# LANGUAGE FlexibleContexts #-}

module AoC2021.Day04 where

import Data.List (partition)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Linear
import Test.HUnit ((@=?))
import Utils

type Point = (Int, Int)

parse :: [Char] -> ([Int], [Map Int Point])
parse = f . splitOn "\n\n"
 where
  f (draws : boards) = (fmap readInt $ splitOn "," draws, fmap toBoard $ boards)
  f s = error $ show s

  toBoard = indexedMap . fmap (fmap readInt . words) . lines

indexedMap :: [[Int]] -> Map Int Point
indexedMap xss = (Map.fromList $ fmap swap q)
 where
  q = [((x, y), w) | (x, xs) <- zip [0 ..] xss, (y, w) <- zip [0 ..] xs]

wins :: [Set Point]
wins = cs <> fmap (Set.map swap) cs
 where
  cs = fmap addCols [0 .. 4]
  addCols x = Set.fromList $ fmap (x,) [0 .. 4]

hasWon :: (a, Set Point) -> Bool
hasWon (_, curs) = any (`Set.isSubsetOf` curs) wins

removeNum :: Int -> (Map Int Point, Set Point) -> (Map Int Point, Set Point)
removeNum d (m, s)
  | (Just xy, m') <- Map.updateLookupWithKey (\_ _ -> Nothing) d m =
    (m', Set.insert xy s)
  | otherwise = (m, s)

playBingo :: (forall a. [a] -> a) -> [Int] -> [(Map Int Point, Set Point)] -> [Int]
playBingo pick (d : ds) boards
  | (winners@(_ : _), rest) <- partition hasWon played
    , let numSum = sum $ Map.keys $ fst $ pick winners =
    d * numSum : playBingo pick ds rest
  | otherwise =
    playBingo pick ds played
 where
  played = fmap (removeNum d) boards
playBingo pick [] b = []

play :: (forall a. [a] -> a) -> ([Int], [Map Int Point]) -> Int
play pick (nums, boards) = pick $ playBingo pick nums (fmap (,mempty) boards)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = play head parsed
  print resA
  resA @=? 27027

  let resB = play last $ parsed
  print resB
  resB @=? 36975