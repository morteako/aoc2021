module AoC2015.Day26 where

import Test.HUnit ((@=?))

parse = id

solve = id

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solve parsed
  print resA

-- let resB = solve parsed
-- print resB

-- 0.00s