module AoC2015.Day14 where

import Control.Lens
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import Test.HUnit ((@=?))

parse :: String -> [(Int, Int, Int)]
parse = fmap (f . words) . lines
 where
  f [_name, _, _, n, _, _, s, _, _, _, _, _, _, r, _] = over each read (n, s, r)
  f s = error $ show s

makeRein :: (Int, Int, Int) -> Int -> Int
makeRein (speed, secs, restSecs) curSec = if m < secs then speed else 0
 where
  m = mod curSec (secs + restSecs)

solveA :: [(Int, Int, Int)] -> Max (Sum Int)
solveA tups = foldMap (\f -> Max $ foldMap (Sum . f) [0 .. 2502]) reins
 where
  reins = map makeRein tups

findHighestIndexes :: [Int] -> [Int]
findHighestIndexes = fst . ifoldr f mempty
 where
  f i a b@(is, Max curMax) = case compare a curMax of
    LT -> b
    EQ -> (i : is, Max curMax)
    GT -> ([i], Max a)

solveB :: [(Int, Int, Int)] -> Int
solveB tups =
  maxCount
    . concatMap findHighestIndexes
    . transpose
    . map (\rein -> scanl1 (+) $ map rein [0 .. 2502])
    $ reins
 where
  reins = map makeRein tups
  maxCount =
    maximum
      . Map.fromListWith (+)
      . map (,1)

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 2655

  let resB = solveB parsed
  print resB
  resB @=? 1059

-- c:0.01, i:0.05s