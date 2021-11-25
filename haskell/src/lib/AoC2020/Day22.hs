module AoC2020.Day22 where

import Control.Lens hiding (Empty, (:<), (|>))
import Data.Either.Extra (fromEither)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

parse :: [Char] -> (Seq Int, Seq Int)
parse = toTuple . fmap (Seq.fromList . map (read @Int) . tail . lines) . splitOn "\n\n"
  where
    toTuple [a, b] = (a, b)

play :: Ord a => (Seq a, Seq a) -> Seq a
play (a, b) = go a b
  where
    go (x :<| xs) (y :<| ys)
      | x > y = go (xs |> x |> y) ys
      | x < y = go xs (ys |> y |> x)
    go xs Empty = xs
    go Empty ys = ys

solve1 :: (Seq Int, Seq Int) -> Int
solve1 = sumWithRevIndex . play

solve2 :: (Seq Int, Seq Int) -> Int
solve2 = sumWithRevIndex . fromEither . playRec

sumWithRevIndex :: Seq Int -> Int
sumWithRevIndex = ifoldMapByOf (reversed . reindexed (+ 1) ifolded) (+) 0 (*)

playRec :: (Seq Int, Seq Int) -> Either (Seq Int) (Seq Int)
playRec (a, b) = go Set.empty a b
  where
    go :: Set.Set (Seq Int, Seq Int) -> Seq Int -> Seq Int -> Either (Seq Int) (Seq Int)
    go prev xs ys
      | Set.member (xs, ys) prev =
        Left xs
    go prev xs'@(x :<| xs) ys'@(y :<| ys)
      | length xs >= x && length ys >= y =
        case go prev (Seq.take x xs) (Seq.take y ys) of
          Left _ -> leftGo
          Right _ -> rightGo
      | x > y = leftGo
      | x < y = rightGo
      where
        prev' = Set.insert (xs', ys') prev
        leftGo = go prev' (xs |> x |> y) ys
        rightGo = go prev' xs (ys |> y |> x)
    go prev xs Empty = Left xs
    go prev Empty ys = Right ys

run xs = do
  let parsed = parse xs
  print $ solve1 parsed -- 30138
  print $ solve2 parsed -- 31587
