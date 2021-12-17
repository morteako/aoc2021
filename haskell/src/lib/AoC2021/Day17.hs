module AoC2021.Day17 where

import Control.Lens
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Utils (readInt)

toTup [x, y] = (x, y)

parse :: [Char] -> Maybe ((Int, Int), (Int, Int))
parse xs = do
  rest <- stripPrefix "target area: " xs
  let xy = splitOn ", " rest
  let f = toTup . fmap readInt . splitOn ".." . drop 2
  let [x, y] = fmap f xy
  Just (x, y)

step (PosVel pos vel) = PosVel (pos + vel) (V2 toZero pred <*> vel)

_pos (PosVel p _) = p

solveA limits = maximumOf (folded . folded . to _pos . _y) $ mapMaybe (findInside limits) $ map (\x -> iterate step $ (PosVel 0 x)) velos
 where
  velos = V2 <$> [(-100) .. 100] <*> [(-100) .. 100]

data PosVel = PosVel (V2 Int) (V2 Int) deriving (Show)

solveB limits = length $ mapMaybe (findInside limits) $ map (\x -> iterate step $ (PosVel 0 x)) velos
 where
  velos = V2 <$> [0 .. 1000] <*> [(-200) .. 1000]

inRange ((x, x'), (y, y')) (V2 px py) =
  x <= px && px <= x' && y <= py && py <= y'

gone ((x, x'), (y, y')) (PosVel pos@(V2 px py) vel@(V2 vx vy)) = vy < 0 && py < y

findInside range (PosVel pos vel : xs) | inRange range pos = Just $ [PosVel pos vel]
findInside range (pv@(PosVel pos vel) : xs) | gone range pv = Nothing
findInside range (x : xs) = (x :) <$> findInside range xs
findInside range [] = Nothing

toZero n = case compare n 0 of
  LT -> n + 1
  EQ -> 0
  GT -> n -1

run :: String -> IO ()
run xs = do
  let Just parsed = parse xs

  let resA = solveA parsed
  print resA

  let resB = solveB parsed
  print resB
  resB @=? 3344