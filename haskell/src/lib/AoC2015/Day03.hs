module AoC2015.Day03 where

import Control.Lens (both, to, view)
import Data.Either (partitionEithers)
import qualified Data.Set as Set
import Linear.V2 (V2 (V2))

parse :: [Char] -> [V2 Int]
parse = fmap f
 where
  f '^' = V2 0 1
  f 'v' = V2 0 (-1)
  f '<' = V2 1 0
  f '>' = V2 (-1) 0
  f c = error $ show c

solveA :: [V2 Int] -> Int
solveA = Set.size . gen

gen :: [V2 Int] -> Set.Set (V2 Int)
gen = Set.fromList . scanl (+) (V2 0 0)

solveB :: [V2 Int] -> Int
solveB = Set.size . view (both . to gen) . partitionEithers . zipWith ($) (cycle [Left, Right])

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let a = solveA parsed
  let b = solveB parsed
  print a
  print b
