module AoC2021.Day18 where

import Data.Char

import Control.Monad
import Data.List (stripPrefix, uncons)
import Test.HUnit ((@=?))
import Utils (readInt)

data SnailFishP = Leaf Int | Node (SnailFishP, SnailFishP) deriving (Show, Read)
data SnailFish = L Int | N SnailFish SnailFish deriving (Read, Eq)

simp :: SnailFishP -> SnailFish
simp (Leaf i) = L i
simp (Node (a, b)) = N (simp a) (simp b)

parse :: String -> [SnailFish]
parse = fmap parseOne . lines

parseOne :: [Char] -> SnailFish
parseOne = simp . read @SnailFishP . foldMap f
 where
  f '[' = "Node("
  f ']' = ")"
  f d | isDigit d = "Leaf " ++ pure d
  f x = [x]

splitP :: SnailFish -> Maybe SnailFish
splitP (L i) | i >= 10 = let (d, n) = quotRem i 2 in Just $ N (L d) (L $ d + n)
splitP (L i) = Nothing
splitP (N l r) = case splitP l of
  Nothing -> N l <$> splitP r
  Just l -> Just $ N l r

findExplo :: SnailFish -> Either SnailFish SnailFish
findExplo s = case indexSnail 0 0 s of
  Left n -> Left s
  Right (target, tl, tr) -> Right $ snd $ fixSnail 0 s
   where
    fixSnail i l@(L li)
      | i == target -1 = (i + 1, L (li + tl))
      | i == target + 1 = (i + 1, L (li + tr))
      | i == target = error "WTF"
      | otherwise = (i + 1, L li)
    fixSnail i q@(N (L l) (L r)) | i == target = (i + 1, L 0)
    fixSnail i (N l r) = case fixSnail i l of
      (i, ll) -> case fixSnail i r of (i, rr) -> (i, N ll rr)
 where
  indexSnail i _ (L _) = Left $ i + 1
  indexSnail i n (N (L l) (L r)) = if n >= 4 then Right (i, l, r) else Left $ i + 2
  indexSnail i n (N l r) =
    case indexSnail i (n + 1) l of
      Left i -> indexSnail i (n + 1) r
      r -> r

untilRight :: SnailFish -> SnailFish
untilRight x =
  case findExplo x of
    Right r -> untilRight r
    Left l -> maybe x (untilRight) $ splitP x

addPair :: SnailFish -> SnailFish -> SnailFish
addPair x y = untilRight $ N x y

solveA :: [SnailFish] -> Int
solveA = magnitude . foldl1 addPair

solveB :: [SnailFish] -> Int
solveB xs = maximum $
  fmap magnitude $ do
    x <- xs
    y <- xs
    guard (x /= y)
    pure $ addPair x y

magnitude :: SnailFish -> Int
magnitude (L i) = i
magnitude (N l r) = 3 * magnitude l + 2 * magnitude r

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  print resA
  resA @=? 4088

  let resB = solveB parsed
  print resB
  resB @=? 4536
