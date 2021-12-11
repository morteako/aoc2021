{-# LANGUAGE AllowAmbiguousTypes #-}

module AoC2021.Day11 where

import Control.Applicative
import Control.Arrow
import Control.Lens (FoldableWithIndex (ifoldMap), view)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)

index :: [b] -> [(Int, b)]
index = zip [0 ..]

type OctoMap = Map.Map (V2 Int) Octo

parse :: String -> OctoMap
parse = Map.fromList . concatMap f . index . fmap index . (fmap . fmap) (Un . digitToInt) . lines
 where
  f (x, xs) = fmap (g x) xs
  g x (y, xs) = (V2 x y, xs)

neighs :: V2 Int -> [V2 Int]
neighs xy = drop 1 $ do
  x <- [0, 1, (-1)]
  y <- [0, 1, (-1)]
  pure $ xy + (V2 x y)

data Octo = Un Int | Flash | OldFlash deriving (Eq)

oldToUn :: Octo -> Octo
oldToUn OldFlash = Un 0
oldToUn x = x

flashToOld :: Octo -> Octo
flashToOld Flash = OldFlash
flashToOld x = x

incOcto :: Octo -> Octo
incOcto (Un i) = if i == 9 then Flash else Un (i + 1)
incOcto x = x

incAll :: Map.Map k Octo -> Map.Map k Octo
incAll = Map.map incOcto

step :: (OctoMap -> OctoMap) -> OctoMap -> OctoMap
step f (f -> newGrid)
  | null flashes = fmap oldToUn newGrid
  | otherwise = step id updated
 where
  flashes = Map.filter (== Flash) newGrid
  ns = ifoldMap (\v _ -> neighs v) flashes

  updated = foldl' (\acc c -> Map.adjust incOcto c acc) (fmap flashToOld newGrid) ns

steps :: OctoMap -> [OctoMap]
steps = iterate (step incAll)

solveA :: OctoMap -> Int
solveA = sum . map countFlashes . take 100 . drop 1 . steps
 where
  countFlashes = Map.size . Map.filter (== Un 0)

solveB :: OctoMap -> Maybe Int
solveB = findIndex allFlashes . steps
 where
  allFlashes = all (== Un 0)

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 1681

  let resB = solveB parsed
  print resB
  resB @=? Just 276
