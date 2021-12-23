{-# LANGUAGE FlexibleContexts #-}

module AoC2021.Day20 where

import AoC2021.Day17 (toTup)
import Control.Arrow
import Control.Lens
import Data.Digits (unDigits)
import Data.Foldable
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as Set
import Debug.Trace
import Linear

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map.Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold (V2 Int) String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

fromChar :: Char -> Maybe Int
fromChar '#' = Just 1
fromChar '.' = Just 0
fromChar _ = Nothing

neighs :: V2 Int -> [V2 Int]
neighs xy = do
  x <- [1, 0, (-1)]
  y <- [1, 0, (-1)]
  pure $ V2 x y + xy

traceShowF f x = traceShow (f x) x

-- lookupGridNum :: Map.Map Int b -> Map.Map (V2 Int) Int -> V2 Int -> b
lookupGridNum bitmap grid v =
  v
    & neighs
    & fmap (\x -> if Set.member x grid then 1 else 0)
    -- & traceShowF length
    & unDigits 2
    & (bitmap Map.!)

parse :: [Char] -> (Map.Map Int Int, Set.Set (V2 Int))
parse = (f *** toSet . parseAsciiMap fromChar) . toTup . splitOn "\n\n"
 where
  toSet = Map.keysSet . Map.filter (== 1)
  f = Map.fromList . zip [0 ..] . mapMaybe fromChar

solveA (bitMap, grid) = take 3 $ iterate enhance grid
 where
  -- l = lookupGridNum bitMap grid (V2 2 2)
  makePoints (getMins -> (Min x, Min y, Max x', Max y')) = V2 <$> [x -4 .. x' + 4] <*> [y -4 .. y' + 4]

  enhance g = foldMap (\k -> if lookupGridNum bitMap g k == 1 then Set.singleton k else mempty) $ makePoints g

getMins = foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y))

-- dsucc = succ . succ
-- dpred = pred . pred

-- gg = over _4 dsucc . over _3 dsucc . over _2 dpred . over _1 dpred

-- makeEdges (getMins -> mins) = makeEdge (gg succ pred mins) <> makeEdge (gg dsucc dpred mins)

-- makeEdge ((Min x, Min y, Max x', Max y')) = Map.fromList $ fmap (,0) $ xs ++ ys
--  where
--   xs = fmap (V2 x) [y .. y'] ++ fmap (V2 x') [y .. y']
--   ys = fmap (flip V2 y) [x .. x'] ++ fmap (flip V2 y') [x .. x']

findBounds ::
  (Foldable t, R2 f) =>
  -- | The points in 2D space
  t (f Int) ->
  -- | V2 (V2 minX minY) (V2 maxX maxY)
  (Int, Int, Int, Int)
findBounds cs = (getMin minX, getMin minY, getMax maxX, getMax maxY)
 where
  (minX, minY, maxX, maxY) = foldMap f cs
  f point =
    ( Min $ view _x point
    , Min $ view _y point
    , Max $ view _x point
    , Max $ view _y point
    )

display :: Foldable t => t (V2 Int) -> IO ()
display points = do
  print "-------------"
  let (minx, miny, maxx, maxy) = findBounds points
  for_ [miny .. maxy] $ \y -> do
    for_ [minx .. maxx] $ \x -> do
      if V2 x y `elem` points
        then -- then putStr "▓"
          putStr "#"
        else putStr "."
    putStrLn ""

run :: String -> IO ()
run xs = do
  let parsed = parse xs

  let resA = solveA parsed
  -- mapM print $ Map.toList resA
  mapM display resA
  print $ resA

-- print $ neighs (V2 5 10)