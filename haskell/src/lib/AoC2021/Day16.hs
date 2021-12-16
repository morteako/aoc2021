module AoC2021.Day16 where

import Control.Lens hiding (index)
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Digits
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Debug.Trace
import Linear
import Numeric.Lens (hex)
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: String -> [Int]
parse = foldMap hexToBits

hexToBits '0' = [0, 0, 0, 0]
hexToBits '1' = [0, 0, 0, 1]
hexToBits '2' = [0, 0, 1, 0]
hexToBits '3' = [0, 0, 1, 1]
hexToBits '4' = [0, 1, 0, 0]
hexToBits '5' = [0, 1, 0, 1]
hexToBits '6' = [0, 1, 1, 0]
hexToBits '7' = [0, 1, 1, 1]
hexToBits '8' = [1, 0, 0, 0]
hexToBits '9' = [1, 0, 0, 1]
hexToBits 'A' = [1, 0, 1, 0]
hexToBits 'B' = [1, 0, 1, 1]
hexToBits 'C' = [1, 1, 0, 0]
hexToBits 'D' = [1, 1, 0, 1]
hexToBits 'E' = [1, 1, 1, 0]
hexToBits 'F' = [1, 1, 1, 1]
hexToBits _ = undefined

toInt = unDigits 2

groupBits xs =
  let (v, rest) = splitAt 3 xs
      (t, rest') = splitAt 3 rest
      packtype = toPackType t
      packfunc = toPackfunc packtype
   in (toInt v, packtype, packfunc rest')

data PackType = Literal | Operator deriving (Show)

toPackType :: [Int] -> PackType
toPackType (toInt -> 4) = Literal
toPackType (toInt -> x) = Operator

toPackfunc Literal rest = (toInt . foldMap tail $ packs, drop (traceShowId toDrop) $ concat strings)
 where
  toDrop = length (concat packs) `mod` 4
  (packs, strings) = getPackets $ chunksOf 5 rest
  long x = length x >= 5
toPackfunc Operator (x : xs) = (toInt . foldMap tail $ packs, concat strings)
 where
  toDrop = length (concat packs) `mod` 4
  n = if x == 0 then 11 else 15
  (m, rest) = splitAt n xs
  (packs, strings) = getPackets $ chunksOf n rest
toPackfunc _ _ = undefined

-- toPackfunc Operator = toInt . tail

getPackets = go []
 where
  -- go xs [] = (reverse xs,[])
  go xs ((y : ys) : rest) = if y == 0 then (reverse $ (y : ys) : xs, rest) else go ((y : ys) : xs) rest
  go _ _ = undefined

solveA = groupBits

run :: String -> IO ()
run xs = do
  let parsed = parse "D2FE28"

  let resA = solveA parsed
  print resA

  print $ break (== 1) [2, 3, 4, 1, 4, 5]