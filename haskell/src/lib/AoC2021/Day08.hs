-- {-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- {-# OPTIONS_GHC -fdefer-type-errors #-}
-- {-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}

module AoC2021.Day08 where

import Control.Arrow
import Data.Foldable

import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as Set
import Debug.Trace
import Control.Monad

-- solveA xs = foldMap (Sum . length . id . filter (\x -> length x `elem` uniqLengths) . fmap id . snd) xs
--  where

-- uniqLengths = fmap (length . (digits Map.!)) [1, 4, 7, 8]

parse :: String -> [([String], [String])]
parse = fmap (f . fmap words . splitOn "|") . lines
 where
  f [x, y] = (x, y)
  f xs = error $ show xs

(#) :: Eq a => [a] -> [a] -> [a]
(#) = intersect

type S = Set.Set Char

tra = id

digs = ["abcefg" :: String, "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

indDigs = Map.fromList $ flip zip ['0'..] ["abcefg" :: String, "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

lendigits =
  Map.fromListWith (++) $ fmap (length &&& (: []) . Set.fromList) digs

-- poss :: [String] -> [Map.Map Char S]
-- poss strGroup = do
--   let l = length $ head strGroup
--   str <- strGroup
--   tra $ fmap (\x -> Map.fromList $ fmap (\c -> (c, x)) str) $ tra (lendigits Map.! length str)

-- figureDec xs = fmap (Map.unionsWith (Set.intersection)) $ sequenceA $ fmap poss xs

figureDec2 xs = fmap (Map.unionsWith (Set.intersection)) $ sequenceA $ fmap poss2 xs

-- solveB = fmap (makePretty . sort) . mapMaybe (check . tra . figureDec . tra . fst)
-- figRec m (gs:gss) = 

-- solveBTessts :: [([String], [String])] -> _
solveBTessts = sum . map f
  where
   
-- f :: ([String], [String]) -> [String]
f :: ([String], [String]) -> Int
f (xs,digs) = read . map (indDigs Map.!) $ fmap (sort . fmap (lookDig Map.!)) digs
  where
    -- getC = fmap (indDigs Map.!) 
    lookDig = head . (fmap) (Map.fromList . makePretty) . id . mapMaybe check . figureDec2 . tra . groupOn length . sortOn length $ xs
    -- findDigit

makePretty = fmap (fmap f) . sort
 where
  f s = if Set.size s == 1 then Set.findMin s else error $ show s


poss2 strGroup = fmap (Map.unionsWith (Set.intersection)) $ do
  p <- permutations strGroup
  let l = length $ head strGroup
  pure $ do
    str <- strGroup
    zipWith (\cur target -> Map.fromList $ fmap (\c -> (c, target)) cur) p (lendigits Map.! l)
    -- tra $ fmap (\x -> (str,x)) $ tra (lendigits Map.! l)

check :: Map.Map Char S -> Maybe [(Char, S)]
check xs
  -- | trace (": " ++ show xs) False = Nothing
  | Map.null xs = Just []
  | Map.null ones = Nothing
  | otherwise = fmap (os ++) $ check $ foldl' (\acc o -> fmap (flip Set.difference o) acc) rest $ fmap snd os
 where
  (ones, rest) = Map.partition (\x -> length x == 1) xs
  os = Map.toList ones

(%) = (,)

run :: String -> IO ()
run xs = do
  -- let xs = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf"

  let parsed = parse xs
  -- print parsed

  -- let resA = solveA parsed
  -- print resA
  print lendigits

  -- print $ poss "abcde"

  -- print $ check $ Map.fromList ['a' % "abc", 'b' % "b", 'c' % "bc"]

  let resB = solveBTessts parsed
  print resB
