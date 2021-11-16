module AoC2015.Day15 where

import Control.Applicative (ZipList (ZipList))
import Data.Foldable (Foldable (fold))
import Data.Map (Map)
import Data.Monoid
import Test.HUnit ((@=?))

parse = fmap (f . words) . lines . filter (not . (== ','))
 where
  f [_, _, c, _, d, _, fl, _, t, _, cal] = (fmap (read @Int) [c, d, fl, t], read @Int cal)
  f xs = error $ show xs

getCombs :: [[Int]]
getCombs = combs 0 100 4
 where
  combs c 0 xs = [[c]]
  combs _ n 1 = [[n]]
  combs c n m = (combs (c + 1) (n -1) m) ++ fmap (c :) (combs 0 n (m -1))

newtype M a = M {getM :: [a]}
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via Ap ZipList (Sum a)
  deriving (Foldable, Functor)

solveA :: [([Int], Int)] -> Int
solveA (fmap fst -> ingrs) = maximum $ getIngrProduct <$> getCombs
 where
  getIngrProduct w = product . fmap (max 0) . fold . zipWith (\x -> M . fmap (* x)) w $ ingrs

solveB :: [([Int], Int)] -> Int
solveB ingrs = maximum $ fmap (product . tail) . filter (\x -> head x <= 500) $ fmap (getM . getIngrSum) $ getCombs
 where
  is = fmap (\(xs, x) -> x : xs) ingrs
  getIngrSum curComb = fmap (max 0) . fold $ zipWith (\c i -> M $ fmap (* c) i) curComb $ is

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  18965440 @=? resA

  let resB = solveB parsed
  print resB
  15862900 @=? resB

-- i : 2.6 s (fast i with toplevel val instead of getCCombs func?? check all 3 ccombs compiledd)
-- c : 0.6s