module AoC2015.Day01 where

import Control.Lens
import Control.Monad (guard, replicateM)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Semigroup
import Data.Sequence (foldMapWithIndex)

data Dir = Down | Up deriving (Show, Eq)

solveA :: [Dir] -> Int
solveA = sum . map f

f :: Num p => Dir -> p
f Down = -1
f Up = 1

solveB :: [Dir] -> Int
solveB = length . takeWhile (>= 0) . scanl (+) 0 . map f

-- solveB :: [Dir] -> Int
solveBMon = ifoldMap q . getH . foldMap @[] (H . pure . f)
 where
  q i n = if n < 0 then Just (First $ i + 1) else Nothing

newtype H = H {getH :: [Int]} deriving (Show)

instance Semigroup H where
  H [] <> ys = ys
  H (x : xs) <> H ys = H $ x : xs ++ fmap (+ x) ys

instance Monoid H where
  mempty = H []

parse :: [Char] -> [Dir]
parse = map f
 where
  f '(' = Up
  f ')' = Down
  f x = error $ pure x

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print parsed
  let a = solveA parsed
  let b = solveB parsed
  print a
  print b
  print $ solveBMon parsed
