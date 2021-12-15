module AoC2021.Day14 where

import Data.List.Extra (splitOn, word1)
import qualified Data.Map as Map
import Debug.Trace
import Test.HUnit ((@=?))

parse :: [Char] -> (String, Map.Map (Char, Char) Char)
parse = f . splitOn "\n\n"
 where
  f [start, lines -> inserts] = (start, Map.fromList $ fmap (toTup . splitOn " -> ") inserts)

  toTup [[a, a'], b] = ((a, a'), head b)

stepFast :: Map.Map (Char, Char) Integer -> Map.Map (Char, Char) Char -> Map.Map (Char, Char) Integer
stepFast cur m = Map.fromListWith (+) $ Map.foldMapWithKey update cur
 where
  update (a, b) n = let w = m Map.! (a, b) in [((a, w), n), ((w, b), n)]

makeMap :: [Char] -> Map.Map (Char, Char) Integer
makeMap = Map.fromListWith (+) . fmap (,1) . (zip <*> tail)

solve :: ([Char], Map.Map (Char, Char) Char) -> [Integer]
solve (start, insertions) = fmap (findMaxMin . insertLast . tostring) . iterate (flip stepFast insertions) . makeMap $ start
 where
  insertLast = Map.adjust (+ 1) (last start)
  findMaxMin xs = maximum xs - minimum xs
  tostring = Map.mapKeysWith (+) fst

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let steps = solve parsed

  let resA = steps !! 10
  print resA
  resA @=? 2797

  let resB = steps !! 40
  print resB
  resB @=? 2926813379532

-- mapM_ (print . tostring) resB
