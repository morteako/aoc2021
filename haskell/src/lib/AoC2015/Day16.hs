module AoC2015.Day16 where

import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit ((@=?))

parse :: String -> [(Int, Map [Char] Int)]
parse = fmap (makeSue . fmap f . chunksOf 2 . words . filter (flip notElem (",:" :: String))) . lines
 where
  f [k, n] = (k, read @Int n)
  f xs = error $ show xs

  makeSue (("Sue", n) : m) = (n, Map.fromList m)
  makeSue m = error $ show m

actualSue :: Map [Char] (Int -> Bool)
actualSue =
  (==)
    <$> Map.fromList
      [ ("children", 3)
      , ("cats", 7)
      , ("samoyeds", 2)
      , ("pomeranians", 3)
      , ("akitas", 0)
      , ("vizslas", 0)
      , ("goldfish", 5)
      , ("trees", 3)
      , ("cars", 2)
      , ("perfumes", 1)
      ]

solveA :: [(c, Map [Char] Int)] -> c
solveA = fst . head . filter (isSue . snd)
 where
  isSue = and . Map.intersectionWith ($) actualSue

solveB :: [(c, Map [Char] Int)] -> c
solveB = fst . head . filter (isSue . snd)
 where
  isSue = and . Map.intersectionWith ($) (updatedSueFacts <> actualSue)
  updatedSueFacts =
    Map.fromList
      [ ("cats", (> 7))
      , ("pomeranians", (< 3))
      , ("goldfish", (< 5))
      , ("trees", (> 3))
      ]

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 213

  let resB = solveB parsed
  print resB
  resB @=? 323

-- c: 0.00 s
-- i: 0.01 s