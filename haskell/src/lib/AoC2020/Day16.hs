module AoC2020.Day16 where

import Control.Lens
import Control.Monad
import Data.List
import Data.List (transpose)
import Data.List.Split
import qualified Data.Set as Set
import Debug.Trace

type IntPair = (Int, Int)

type RangesYourAndTickets = ([(String, [IntPair])], [Int], [[Int]])

parse :: [Char] -> RangesYourAndTickets
parse xs = case splitOn "\n\n" $ xs of
  [ranges, head . drop 2 . words -> myTicket, nearbyTickets] -> (map parseProp $ lines ranges, read <$> splitOn "," myTicket, parseTickets nearbyTickets)
 where
  parseProp (splitOn ": " -> [name, splitOn " or " -> [r1, r2]]) = (name, [parseRange r1, parseRange r2])
  parseRange (splitOn "-" -> [n1, n2]) = (read n1, read n2)
  parseTickets = fmap (fmap read . splitOn ",") . tail . lines

inRange x (a, b) = a <= x && x <= b

solve1 :: RangesYourAndTickets -> Int
solve1 (ranges, _, tickets) = sum $ foldMap (filter (\t -> not $ checkInSomeRange t $ concatMap snd ranges)) tickets

checkInSomeRange x ranges = any (inRange x) ranges

solve2 :: RangesYourAndTickets -> Int
solve2 (ranges, yourTicket, tickets) =
  let r = getColumns (ranges, yourTicket, tickets)
      cols = over (traverse . _2) Set.findMin $ findComb r
   in product $ map (yourTicket !!) $ map fst $ filter (\(a, b) -> isPrefixOf "departure" b) cols

getColumns :: RangesYourAndTickets -> [(Int, Set.Set String)]
getColumns (ranges, _, tickets) = zip [0 ..] $ do
  let checkAll = all (flip checkInSomeRange (concatMap snd ranges))
  let filteredTickets = filter checkAll tickets
  ts <- transpose filteredTickets
  let ran = filter (\(_, r) -> all (\t -> checkInSomeRange t r) ts) ranges
  pure $ Set.fromList $ map fst ran

findComb :: [(Int, Set.Set String)] -> [(Int, Set.Set String)]
findComb xs = case rest of
  [] -> ones
  _ -> ones ++ findComb ((over (traverse . _2) removeOld) rest)
 where
  (ones, rest) = partition ((1 ==) . Set.size . snd) xs
  removeOld w = Set.difference w (Set.unions $ fmap snd ones)

run xs = do
  let parsed = parse xs

  print $ solve1 parsed -- 20060
  print $ solve2 parsed -- 2843534243843
