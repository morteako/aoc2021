module AoC2015.Day12 where

import Control.Lens
import Data.Aeson

import Data.Aeson.Lens as AL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.UTF8 as BSlU
import Data.Data.Lens (biplate)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import Test.HUnit ((@=?))

parse :: String -> Value
parse = fromJust . decode . BLU.fromString

solveA :: Value -> Integer
solveA = round . sumOf (biplate @_ @Scientific)

solveB :: Value -> Integer
solveB = round . f
 where
  f :: Value -> Scientific
  f o@(Object q) = sumOf (below p . folded . to f) $ q
  f (Number i) = i
  f a = sumOf (plate . to f) a

  p = prism' id (\x -> if x == "red" then Nothing else Just x)

run :: String -> IO (String, String)
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  let resB = solveB parsed
  print resB

  return mempty

-- c: 0.03s i:0.03s