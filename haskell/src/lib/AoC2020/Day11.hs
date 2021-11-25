{-# LANGUAGE FlexibleContexts #-}

module AoC2020.Day11 where

import Control.Lens hiding (Empty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Linear hiding (trace)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold (V2 Int) String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

parse :: String -> Map (V2 Int) Seat
parse = parseAsciiMap toSeat
 where
  toSeat 'L' = Just Empty
  toSeat '#' = Just Occ
  toSeat '.' = Just Floor
  toSeat _ = Nothing

data Seat = Empty | Occ | Floor
  deriving (Eq)

instance Show Seat where
  show Empty = "L"
  show Occ = "#"
  show Floor = "."

nbs :: V2 Int -> [V2 Int]
nbs v2 = tail $ do
  f <- [id, pred, succ]
  g <- [id, pred, succ]
  pure $ over _x f $ over _y g v2

rundayGen :: Int -> (Map (V2 Int) Seat -> V2 Int -> [Seat]) -> Map (V2 Int) Seat -> Map (V2 Int) Seat
rundayGen lim getNeigs grid = if grid == newGrid then grid else rundayGen lim getNeigs newGrid
 where
  newGrid = Map.mapWithKey f grid
  f point seat = case seat of
    Floor -> Floor
    Empty -> if occNbs == 0 then Occ else Empty
    Occ -> if occNbs >= lim then Empty else Occ
   where
    occNbs = length $ filter (== Occ) (getNeigs grid point)

countOcc = Map.size . Map.filter (== Occ)

solve1 :: Map (V2 Int) Seat -> Int
solve1 = countOcc . rundayGen 4 countClose
 where
  countClose :: Map (V2 Int) b -> V2 Int -> [b]
  countClose grid point = mapMaybe (`Map.lookup` grid) (nbs point)

solve2 :: Map (V2 Int) Seat -> Int
solve2 = Map.size . Map.filter (== Occ) . rundayGen 5 countDiag
 where
  dirs :: V2 Int -> [[V2 Int]]
  dirs v2 = tail $ do
    f <- [id, pred, succ]
    g <- [id, pred, succ]
    let dv = over _x f $ over _y g (V2 0 0)
    [tail $ iterate (+ dv) v2]

  countDiag :: Map (V2 Int) Seat -> V2 Int -> [Seat]
  countDiag grid point = mapMaybe (head . dropWhile (== Just Floor) . map (`Map.lookup` grid)) (dirs point)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve1 parsed --2319
  print $ solve2 parsed --2117
