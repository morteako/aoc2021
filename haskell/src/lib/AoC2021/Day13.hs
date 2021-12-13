module AoC2021.Day13 where

import Control.Lens
import Data.Foldable (Foldable (foldl'), for_)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)

data Fold = OnX Int | OnY Int deriving (Show)

parse :: [Char] -> (Set.Set (V2 Int), [AoC2021.Day13.Fold])
parse = f . fmap lines . splitOn "\n\n"
 where
  f [coords, folds] = (Set.fromList $ fmap toCoord coords, fmap toFold folds)
  f _ = undefined
  toCoord (splitOn "," -> fmap readInt -> [x, y]) = V2 x y
  toCoord _ = undefined
  toFold (stripPrefix "fold along " -> Just (splitOn "=" -> [xy, readInt -> n])) =
    case xy of
      "x" -> OnX n
      "y" -> OnY n
      _ -> undefined
  toFold _ = undefined

flipNums :: Int -> Int -> Int
flipNums n num | num < n = num
flipNums n num = abs (num - 2 * n)

foldSheet :: Set.Set (V2 Int) -> AoC2021.Day13.Fold -> Set.Set (V2 Int)
foldSheet m f = Set.map mod $ Set.filter l m
 where
  mod = case f of
    OnX i -> over _x (flipNums i)
    OnY i -> over _y (flipNums i)

  l = case f of
    OnX i -> (/= i) . view _x
    OnY i -> (/= i) . view _y

solveA :: (Set.Set (V2 Int), [AoC2021.Day13.Fold]) -> Int
solveA (m, folds) = Set.size $ foldSheet m $ head folds

solveB :: Foldable t => (Set.Set (V2 Int), t AoC2021.Day13.Fold) -> Set.Set (V2 Int)
solveB (m, folds) = foldl' foldSheet m folds

printGrid m = do
  for_ [0 .. 50] $ \x -> do
    for_ [0 .. 50] $ \y -> do
      let a = if Set.member (V2 x y) m then '#' else ' '
      putStr [a]
    putStrLn ""

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  resA @=? 827

  let resB = solveB parsed
  printGrid resB
