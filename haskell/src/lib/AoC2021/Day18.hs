module AoC2021.Day18 where

import Control.Lens (both, over)
import Data.Char

import Control.Monad.State
import Data.List (stripPrefix, uncons)
import Data.List.Extra
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace
import Test.HUnit ((@=?))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Read (readMaybe)
import Utils (readInt)

toTup [x, y] = (x, y)
toTup xs = error $ show xs

data SnailFish = Leaf Int | Node (SnailFish, SnailFish) deriving (Show, Read)

newtype Depth = Depth Int deriving (Num, Eq, Ord)

data Snail = Snail Id Depth Int Id deriving (Show)

newtype Id = Id [Int]

instance Show Id where
  show (Id xs) = foldMap show xs

instance Show Depth where
  show (Depth d) = "d" ++ show d

simplify n (Leaf i) = [(Depth n, i)]
simplify (succ -> n) (Node (l, r)) = simplify n l ++ simplify n r

newId :: State Int Id
newId = do
  modify (+ 1)
  fmap (Id . pure) get

simplifySnail :: Id -> Int -> SnailFish -> State Int [Snail]
simplifySnail p n (Leaf i) = do
  ni <- newId
  pure [Snail ni (Depth n) i p]
simplifySnail p (succ -> n) (Node (l, r)) = do
  i <- newId
  ls <- simplifySnail i n l
  rs <- simplifySnail i n r
  pure $ ls ++ rs

simplifyAll = flip evalState 0 . traverse @[] startSimpl
 where
  startSimpl xs = do
    ni <- newId
    simplifySnail ni 0 xs

-- parse :: String -> [SnailFish]
parse = simplifyAll . fmap parseOne . lines

parseOne = read @SnailFish . id . foldMap f
 where
  f '[' = "Node("
  f ']' = ")"
  f d | isDigit d = "Leaf " ++ pure d
  f x = [x]

splittable (Snail _ _ i _) = i >= 10

splitIt (Snail i ((+ 1) -> d) x p) = let (di, n) = quotRem x 2 in [Snail (icons 0 i) d di p, Snail (icons 1 i) d (di + n) p]

icons x (Id xs) = Id $ x : xs

explodable (Depth n, _) = n >= 5

addFirst :: Int -> [Snail] -> [Snail]
addFirst n (Snail i d x p : rest) = Snail i d (x + n) p : rest
addFirst n [] = []

addFirstRev :: Int -> [Snail] -> [Snail]
addFirstRev n xs = case unsnoc xs of
  Nothing -> []
  Just (xs, Snail i d x p) -> xs ++ [Snail i d (x + n) p]

splitPair xs =
  let (l, r) = break splittable xs
   in case r of
        [] -> Nothing
        (r : rs) -> Just $ l ++ splitIt r ++ rs

explodePair xs =
  let (l, r) = breakExplo xs
   in case r of
        [] -> Nothing
        [x] -> error $ show x
        (Snail i d v p : Snail i' d' v' p' : rs) ->
          Just $
            addFirstRev v l ++ [Snail i (d -1) 0 p] ++ addFirst v' rs

breakExplo = go []
 where
  go xs [] = (reverse xs, [])
  go xs [y] = (reverse (y : xs), [])
  go xs ys'@(y : y' : ys) = if isExploPair y y' then (reverse xs, ys') else go (y : xs) (y' : ys)

isExploPair (Snail _ d _ _) (Snail _ d' _ _) = d >= 5 && d == d'

incDepth (Snail i d x p) = (Snail i (d + 1) x p)

addPair l r = untilRight (fmap incDepth $ l ++ r)

untilRight x =
  case explodePair x of
    Just r -> untilRight r
    Nothing -> maybe x untilRight $ splitPair x

-- solveA' = map (addAll 7)

solveA = foldl1 addPair

(#) = (,)

-- getPairs = foldMap (chunksOf 2) . groupOn (\(Depth x, _) -> x)

data DPair = DPair Depth Int Int

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  -- print parsed
  let resA = solveA parsed
  -- mapM_ print $ solveA' parsed
  print resA

-- print $ solveA $ parse "[1,1]\n[2,2]\n[3,3]\n[4,4]"

-- print $ splitPair [0 # 10, 0 # 12]
-- print $ splitPair [0 # 5, 1 # 12, 2 # 9]

-- print $ explodePair $ parseOne "[[[[[9,8],1],2],3],4]"
-- print $ explodePair $ parseOne "[[6,[5,[4,[3,2]]]],1]"
-- print $ explodePair $ parseOne "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"

-- print $ addPair (parseOne "[[[[4,3],4],4],[7,[[8,4],9]]]") (parseOne "[1,1]")

-- print $ splitPair $ fromJust $ explodePair $ fromJust $ explodePair $ parseOne "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
