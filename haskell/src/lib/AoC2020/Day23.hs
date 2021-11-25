{-# LANGUAGE FlexibleContexts #-}

module AoC2020.Day23 where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Semigroup
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Debug.Trace

parse = Seq.fromList . map digitToInt . filter isDigit

len :: Int
len = 9

rotate (x :<| xs) = xs Seq.|> x

maxi = 1000000

move (x :<| xs) = s <> picked <> ss Seq.|> x
 where
  insertIndex = findClosest x picked rest
  (s, ss) = Seq.splitAt (insertIndex + 1) rest
  (picked, rest) = Seq.splitAt 3 xs

findClosest target picked xs = fromJust $ Seq.elemIndexL w xs
 where
  w = head $ filter (\x -> x > 0 && notElem x picked) [target - 1, target - 2, target - 3, target - 4, maxi, maxi - 1, maxi - 2]

solve1 = foldMap show . take 8 . tail . dropWhile (/= 1) . cycle . toList . (!! 100) . iterate move

solve2 s = take 2 . tail . dropWhile (/= 1) . cycle . traceShow "WTF" . toList . (!! 10000000) . iterate move $ new
 where
  new = s <> m
  m = Seq.fromList $ [maximum s + 1 .. 1000000]

solve2Debug s = map findOrg . take 100 . iterate move $ new
 where
  findOrg w = mapMaybe (\x -> Seq.findIndexL (== x) w) $ toList s
  new = s <> m
  m = Seq.fromList $ take 100 [maximum s + 1 ..]

solve2Debug2 s = Set.size . foldl' f Set.empty . take 1000 . iterate move $ new
 where
  findOrg w = mapMaybe (\x -> Seq.findIndexL (== x) w) $ toList s
  f c (findOrg -> n) = if Set.member n c then undefined else Set.insert n c
  new = s <> m
  m = Seq.fromList $ take 100000 [maximum s + 1 ..]

run xs = do
  let parsed = parse xs
  print parsed
  -- mapM_ print $ zip [0 ..] $ take 11 $ iterate move parsed
  -- print $ solve1 parsed -- 97632548
  print $ solve2 parsed

-- print $ Seq.splitAt 3 parsed

-- print $ compare (Left 1) (Right 0)