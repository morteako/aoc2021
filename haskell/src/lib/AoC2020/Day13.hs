module AoC2020.Day13 where

import Control.Lens
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Read (readMaybe)

parse :: String -> (Integer, [Maybe Integer])
parse (lines -> idLine : notes : _) = (read idLine, map readMaybe $ splitOn "," notes)

solve1 :: (Ord a, Num a) => (a, [Maybe a]) -> a
solve1 (id, catMaybes -> busses) = busId * (time - id)
 where
  (time, busId) = minimum $ do
    bus <- busses
    let m = head . dropWhile (id >=) $ iterate (+ bus) bus
    pure (m, bus)

solve2 :: Foldable f => (a, f (Maybe Integer)) -> Integer
solve2 (_, maybeBusses) = solveFast $ over (traverse . _1) toInteger $ itoListOf (folded . _Just) maybeBusses

solveFast :: [(Integer, Integer)] -> Integer
solveFast busses = chinese inds moods - maxInds
 where
  (inds, moods) = unzip $ map fixInd busses
  fixInd (i, x) = (abs (i - maxInds) `mod` x, x)
  maxInds = maximum $ map fst busses

chinese :: [Integer] -> [Integer] -> Integer
chinese inds mods = flip mod nProd $ sum $ zipWith f inds mods
 where
  f a1 ni = let bi = div nProd ni in a1 * bi * modInv bi ni

  nProd = product mods

modInv :: Integer -> Integer -> Integer
modInv a m
  | 1 == g = mkPos i
  | otherwise = error "wtf"
 where
  (i, _, g) = gcdExt a m
  mkPos x
    | x < 0 = x + m
    | otherwise = x

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
   in (t, s - q * t, g)

run xs = do
  let parsed = parse xs

  print $ solve1 parsed
  print $ solve2 parsed
