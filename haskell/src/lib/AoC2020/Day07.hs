module AoC2020.Day07 where

import Data.Function
import Data.List.Extra (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Sum (Sum))
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Map.Map String [(Int, String)]
parse xs = foldMap (f . words) $ splitOn "." xs
 where
  f [] = mempty
  f (a : b : "bags" : "contain" : other)
    | unwords other == "no other bags" = mempty
    | otherwise = Map.unionsWith (++) $ map (to (a ++ b)) (chunksOf 4 other)
  f xs = error $ show xs

  to key [n, bag, bag', _] = Map.singleton key [(read n :: Int, bag ++ bag')]
  to _ xs = error $ show xs

flipMap :: Ord v => Map.Map k [(tag, v)] -> Map.Map v [(tag, k)]
flipMap = Map.fromListWith (++) . foldMap pair . Map.toList
 where
  pair (k, vs) = map (f k) vs
  f k (tag, v) = (v, [(tag, k)])

sumKey :: Ord a => Map a [a] -> a -> Set a
sumKey dict key = foldMap f $ Map.lookup key dict
 where
  f xs = Set.fromList xs <> foldMap (sumKey dict) xs

solve1 :: Map String [(a, String)] -> Int
solve1 = Set.size . flip sumKey "shinygold" . (fmap . fmap) snd . flipMap

solve1' dict = Set.size $ walkTree f id ((fmap . fmap) snd . flipMap $ dict) "shinygold"
 where
  f rec xs = Set.fromList xs <> foldMap rec xs

sumBags :: Map String [(Int, String)] -> String -> Sum Int
sumBags dict key = (foldMap . foldMap) f (Map.lookup key dict)
 where
  f (n, ws) = Sum n + Sum n * sumBags dict ws

walkTree f fm dict key = (foldMap . fm) (f (walkTree f fm dict)) (Map.lookup key dict)

walkTree' comb fm dict g key = (foldMap . fm) (comb g) (Map.lookup key dict)

solve2' dict = fix (walkTree' f foldMap dict) "shinygold"
 where
  f rec (n, ws) = Sum n + Sum n * rec ws

solve2'' dict = walkTree f foldMap dict "shinygold"
 where
  f rec (n, ws) = Sum n + Sum n * rec ws

solve2 :: Map String [(Int, String)] -> Sum Int
solve2 = flip sumBags "shinygold"

run :: String -> IO ()
run xs = do
  let p = parse xs
  print $ solve1 p -- 348
  print $ solve2 p -- 18885
  print $ solve1' p
  print $ solve2' p