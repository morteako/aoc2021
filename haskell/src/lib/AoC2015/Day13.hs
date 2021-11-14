module AoC2015.Day13 where

import Control.Lens
import Data.List (nub, permutations)
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Debug.Trace
import Test.HUnit ((@=?))

parse :: String -> Map (String, String) (Sum Int)
parse = foldMap (f . words) . lines
 where
  f [person1, _would, upOrDown, score, _, _, _, _, _, _, person2] = Map.singleton (person1, init person2) (Sum $ sig upOrDown $ read @Int score)
  f xs = error $ show xs

  sig "gain" = id
  sig "lose" = negate
  sig x = error x

solve :: ([String] -> [String]) -> Map (String, String) (Sum Int) -> Maybe (Sum Int)
solve adder m = maximumOf (folded . to checkScore) perms
 where
  perms = permutations . adder . nub . fmap fst . Map.keys $ m

  checkScore (x : xs) = foldMap (checkCouple) $ zip (x : xs) (xs ++ [x])
  checkScore _ = undefined

  checkCouple :: (String, String) -> Sum Int
  checkCouple (a, b) | a == "me" || b == "me" = 0
  checkCouple (a, b) = m Map.! (a, b) + m Map.! (b, a)

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solve id parsed
  print resA
  resA @=? Just 618

  let resB = solve ("me" :) parsed
  print resB
  resB @=? Just 601

-- c : 0.8s , i: 5.7s