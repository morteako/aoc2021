module AoC2021.Day03 where

import Control.Lens
import qualified Data.Map as Map
import Data.Semigroup (Endo (Endo, appEndo), Sum (Sum))

import Data.Char
import Data.Digits
import Data.Foldable (Foldable (toList))
import Data.List
import qualified Data.List as L
import Data.Word
import Numeric as N
import Test.HUnit ((@=?))

parse = fmap (fmap digitToInt) . lines

-- occs :: [Char] -> Map.Map Char Int
occs = toList . ifoldMap (\k v -> Map.singleton v k) . Map.fromListWith (+) . map (,1)

solve = product . fmap (unDigits 2) . transpose . fmap occs . transpose
 where
  f [x, y] = x

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solve parsed
  print resA
