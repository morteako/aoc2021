module AoC2021.Day12 where

import Control.Lens
import Data.Char (isUpper)
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace
import Test.HUnit ((@=?))

data S = Start | End | Named String deriving (Eq, Ord)

data Cave = Big String | Small S deriving (Eq, Ord)

parse :: String -> Map Cave [Cave]
parse = Map.fromListWith (++) . fmap (over _2 (: [])) . concatMap (swappy . toTup . fmap f . splitOn "-") . lines
 where
  f "start" = Small Start
  f "end" = Small End
  f s@(c : _) = if isUpper c then Big s else Small $ Named s
  f _ = undefined

  swappy xs = [xs, swap xs]

  toTup [x, y] = (x, y)
  toTup _ = undefined

removeStartAndEnds :: Map Cave [Cave] -> Map Cave [Cave]
removeStartAndEnds = Map.delete (Small End) . fmap (delete (Small Start))

findPaths :: Monoid t1 => (Map Cave [Cave] -> t1 -> Cave -> t2) -> Map Cave [Cave] -> t2
findPaths f (removeStartAndEnds -> m) = f m mempty (Small Start)

go :: Foldable t => Map Cave (t Cave) -> Set.Set S -> Cave -> [[Cave]]
go m vis (Small End) = [[Small End]]
go m vis cur
  | let poss = m Map.! cur = map (cur :) $ concatMap f poss
 where
  f cave@(Big s) = go m vis cave
  f cave@(Small s)
    | Set.member s vis = []
    | otherwise = go m (Set.insert s vis) cave

solveA :: Map Cave [Cave] -> Int
solveA = length . findPaths go

go2 m vis (Small End) = [[Small End]]
go2 m vis cur
  | any (>= 3) vis = []
  | Map.size (Map.filter (>= 2) vis) >= 2 = []
  | let poss = m Map.! cur = map (cur :) $ concatMap f poss
 where
  f cave@(Big s) = go2 m vis cave
  f cave@(Small s)
    | otherwise = go2 m (Map.insertWith (+) s 1 vis) cave

solveB :: Map Cave [Cave] -> Int
solveB = length . findPaths go2

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 4104

  let resB = solveB parsed
  print resB
  resB @=? 119760
