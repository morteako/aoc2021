module AoC2015.Day12 where

import Control.Lens
import Data.Aeson

-- Plated instance for Value
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.UTF8 as BLU
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

--cleaner solution : inspired from https://hao.codes/haskell-advent.html

solveB_ :: Value -> Integer
solveB_ =
  round
    . sumOf
      ( cosmosOf
          (plate . filtered (notElemOf (_Object . folded) "red"))
          . _Number
      )

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  resA @=? 156366
  let resB = solveB parsed
  print resB
  resB @=? 96852
  print $ solveB_ parsed

-- c: 0.03s i:0.03s