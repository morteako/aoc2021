module AoC2020.Day12 where

import Data.Foldable (Foldable (foldl'))
import Linear (V2 (..), perp, (^*))

parse = map toAction . lines
 where
  toAction (c : num) = toDir c (read num)
  toDir 'N' = Move (V2 0 1)
  toDir 'S' = Move (V2 0 (-1))
  toDir 'E' = Move (V2 1 0)
  toDir 'W' = Move (V2 (-1) 0)
  toDir 'L' = Rotate . degreeToTurns
  toDir 'R' = Rotate . (4 -) . degreeToTurns
  toDir 'F' = Forward
  toDir _ = error ""

  degreeToTurns n = if mod n 90 == 0 then div n 90 else error "not 90 degrees"

data Action = Forward Int | Rotate Int | Move (V2 Int) Int deriving (Show)

data Dir = N | S | E | W

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

rotate :: Num a => V2 a -> Int -> V2 a
rotate curVect n = iterate perp curVect !! n

solve1 :: [Action] -> Int
solve1 = manhattan . fst . foldl' move (V2 0 0, V2 1 0)

move :: (V2 Int, V2 Int) -> Action -> (V2 Int, V2 Int)
move (curPos, curVect) action = case action of
  Forward i -> (curPos + (curVect ^* i), curVect)
  Move v i -> (curPos + (v ^* i), curVect)
  Rotate n -> (curPos, rotate curVect n)

solve2 :: [Action] -> Int
solve2 = manhattan . fst . foldl' moveWaypoint (V2 0 0, V2 10 1)

moveWaypoint :: (V2 Int, V2 Int) -> Action -> (V2 Int, V2 Int)
moveWaypoint (curPos, waypoint) action = case action of
  Forward i -> (curPos + (waypoint ^* i), waypoint)
  Move v i -> (curPos, waypoint + (v ^* i))
  Rotate n -> (curPos, rotate waypoint n)

run xs = do
  let parsed = parse xs
  print $ solve1 parsed -- 319
  print $ solve2 parsed -- 50157
