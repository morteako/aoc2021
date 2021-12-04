{-# LANGUAGE FlexibleContexts #-}

module AoC2021.Day04 where

import Control.Lens
import Data.Char (digitToInt)
import Data.Digits (unDigits)
import Data.List (find, sortOn, transpose)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)
import Linear
import Test.HUnit ((@=?))
import Utils

indexedMap xss = (Map.fromList $ fmap swap q)
 where
  q = [((x, y), w) | (x, xs) <- zip [0 ..] xss, (y, w) <- zip [0 ..] xs]

wins = cs <> fmap (Set.map swap) cs
 where
  cs = fmap addCols [0 .. 4]
  addCols x = Set.fromList $ fmap (x,) [0 .. 4]

parse = f . splitOn "\n\n"
 where
  f (draws : boards) = (fmap readInt $ splitOn "," draws, fmap toBoard $ boards)

  toBoard = indexedMap . fmap (fmap readInt . words) . lines

hasWon (_, curs) = any (\x -> x `Set.isSubsetOf` curs) wins

removeNum d (m, s)
  | (Just xy, m') <- Map.updateLookupWithKey (\_ _ -> Nothing) d m =
    (m', Set.insert xy s)
  | otherwise = (m, s)

-- removeNum d _ = error $ "remove num " ++ show d

-- playBingo :: [Int] -> [(Map.Map Int (Int, Int), Set.Set (Int, Int))] -> Map.Map Int (Int, Int)
playBingo (d : ds) boards
  | Just winner <- find hasWon played = (d, (d *) $ sum $ Map.keys $ fst winner)
  | otherwise = playBingo ds played
 where
  played = fmap (removeNum d) boards
playBingo [] b = error "play1"

solveA (nums, boards) = playBingo nums (fmap (,mempty) boards)

solveB = id

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print parsed
  let resA = solveA parsed
  print resA

-- print $ wins

-- let resB = solveB $ parsed
-- print resB