module AoC2015.Day26 where

import Control.Lens
import Data.Map (Map)
import Data.Map as Map
import Test.HUnit ((@=?))

parse = id

solve = id

run :: String -> IO (String, String)
run xs = do
  let parsed = parse xs
  let resA = solve parsed
  print resA
  let resB = solve parsed
  return mempty

-- 0.00s