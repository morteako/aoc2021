module AoC2015.Day18 where

import Control.Lens
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Linear (R1 (_x), R2 (_y), V2 (..))
import Test.HUnit ((@=?))

parse :: String -> Map.Map (V2 Int) Bool
parse = fmap lightToBool . Map.fromList . itoListOf (reindexed (uncurry $ flip V2) (lined <.> ifolded))
 where
  lightToBool '.' = False
  lightToBool '#' = True
  lightToBool x = error $ show x

neighs xy = drop 1 $ do
  x <- [0, (-1), 1]
  y <- [0, (-1), 1]
  pure $ over _x (+ x) $ over _y (+ y) xy

flickLights :: (Ord a, Num a) => a -> a -> Map.Map (V2 a) Bool -> Map.Map (V2 a) Bool
flickLights stuckX stuckY grid =
  Map.mapWithKey
    ( \k@(V2 x y) v ->
        if (x == stuckX || x == stuckY) && (y == stuckY || y == stuckX)
          then True
          else
            let len = length $ filter id (mapMaybe (grid Map.!?) (neighs k))
             in case v of
                  True -> len == 2 || len == 3
                  False -> len == 3
    )
    grid

solve :: Map.Map (V2 Int) Bool -> Int
solve = length . filter id . toList . (!! 100) . iterate (flickLights (-1) (-1))

solveB :: Map.Map (V2 Int) Bool -> Int
solveB = length . filter id . toList . (!! 100) . iterate (flickLights 0 99) . turnOnCorners
 where
  turnOnCorners = Map.union (Map.fromList $ fmap (,True) [V2 0 0, V2 0 99, V2 99 0, V2 99 99])

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solve parsed
  print resA

  resA @=? 821

  let resB = solveB parsed
  print resB
  resB @=? 886

-- c : 1.64s
-- i : 3.00s