module AoC2020.Day14 where

import Control.Lens
import Control.Lens.Extras
import Data.Bits
import Data.Char
import Data.List.Extra hiding (splitOn)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe

data ParsedCommand = One Int | Zero Int | X Int deriving (Show, Eq, Ord)

data B = BSet Int | BClear Int | BSame Int deriving (Show, Eq, Ord)

parse (lines -> xs) = groups xs
  where
    groups [] = []
    groups (x : xs)
      | isPrefixOf "mask" x,
        let (mems, rest) = span (isPrefixOf "mem") xs =
        (makeMask x, makeMems mems) : groups rest
    makeMask x = imap makeFunc $ reverse $ takeWhileEnd (not . isSpace) x
    makeMems mem = map (f . splitOn " = ") mem
    f ([adr, val]) = (read @Int $ init (drop 4 adr), read @Int val)

    makeFunc i '1' = One i
    makeFunc i '0' = Zero i
    makeFunc i 'X' = X i

runMask l fromB m (mask, mems) = foldl' combine m mems
  where
    combine cur (over l applyMask -> (adr, val)) = Map.insert adr val cur
    applyMask w = foldr fromB w mask

solve1 maskMems = sum $ foldl' (runMask _2 fromB) Map.empty maskMems
  where
    fromB (One i) w = setBit w i
    fromB (Zero i) w = clearBit w i
    fromB (X _) w = w

solve2 maskMems = sum $ foldl' (runMask _1 fromB) Map.empty m
  where
    m = do
      (mask, mems) <- maskMems
      newMasks <- replaceFloating mask
      pure (newMasks, mems)

    fromB (BSet i) w = setBit w i
    fromB (BClear i) w = clearBit w i
    fromB (BSame _) w = w

replaceFloating [] = [[]]
replaceFloating (x : (replaceFloating -> recXs)) = case x of
  X i -> map (BClear i :) recXs ++ map (BSet i :) recXs
  One i -> map (BSet i :) recXs
  Zero i -> map (BSame i :) recXs

run xs = do
  print $ solve1 $ parse xs -- 9628746976360
  print $ solve2 $ parse xs -- 4574598714592
