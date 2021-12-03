module AoC2021.Day02 where

import Control.Lens
import Data.Semigroup (Endo (Endo, appEndo), Sum (Sum))
import Linear
import Test.HUnit ((@=?))
import Utils

-- data Order = Forward Int |

parse = fmap (f . words) . lines
 where
  f [dir, readInt -> n] = case dir of
    "forward" -> V2 n 0
    "down" -> V2 0 n
    "up" -> V2 0 (- n)
    s -> error s
  f s = error $ show s

-- parse2 :: String -> [(V2 Int, Sum Int) -> (V2 Int, Sum Int)]
parse2 = fmap (f . words) . lines
 where
  f [dir, Sum . readInt -> n] = case dir of
    "forward" -> \(V2 x y, aim) -> (V2 (x + n) (y + aim * n), aim)
    "down" -> over _2 (+ n)
    "up" -> over _2 (subtract n)
    s -> error s
  f s = error $ show s

solve = product . fst . ($ (V2 0 0, Sum 0)) . appEndo . foldMap Endo . reverse

q = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

run :: String -> IO ()
run xs = do
  -- let parsed = parse xs
  -- let resA = solve parsed
  -- print resA

  -- resA @=? 1715
  let parsed = parse2 xs
  let resB = solve parsed
  print resB
