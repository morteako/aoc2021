module AoC2015.Day06 where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Data.Foldable
import Data.List (foldl', stripPrefix, uncons)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive.Mutable as VP
import qualified Data.Vector.Unboxed as V
import GHC.Base (RealWorld, coerce)
import Linear (V2 (..))
import Test.HUnit ((@?=))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, digitChar, space, string)

solveA :: [Commands] -> IO (Sum Int)
solveA cs = do
  s <- V.thaw $ V.replicate (1000000) False
  for_ cs $ \(Command c a b) -> do
    for_ (points a b) $ \i -> case c of
      TurnOn -> M.write s i True
      TurnOff -> M.write s i False
      Toggle -> M.modify s not i
  res <- V.freeze s
  pure $ V.foldMap (\x -> if x then Sum 1 else Sum 0) res

solveB :: [Commands] -> IO Int
solveB cs = do
  s <- V.thaw $ V.replicate (1000000) 0
  for_ cs $ \(Command c a b) -> do
    for_ (points a b) $ \i -> case c of
      TurnOn -> M.modify s (+ 1) i
      TurnOff -> M.modify s monus i
      Toggle -> M.modify s (+ 2) i
  res <- V.freeze s
  pure $ V.sum res

data Commands = Command Instr (V2 Int) (V2 Int) deriving (Show)

data Instr = TurnOn | TurnOff | Toggle deriving (Show)

parse :: String -> [Commands]
parse = either (error . show) id . traverse (P.runParser p "") . lines
 where
  p = ps "turn on" TurnOn <|> ps "turn off" TurnOff <|> ps "toggle" Toggle

  ps :: String -> Instr -> P.Parsec () String Commands
  ps s c = do
    string s
    space
    Command c <$> nums <*> (string " through " *> nums)
  nums = do
    i <- read @Int <$> P.many digitChar
    char ','
    i' <- read @Int <$> P.many digitChar
    pure (V2 i i')

monus :: (Ord a, Num a) => a -> a
monus a = max 0 (a - 1)

points :: (Enum b, Num b) => V2 b -> V2 b -> [b]
points (V2 x y) (V2 xx yy) = do
  xs <- [x .. xx]
  ys <- [y .. yy]
  pure (1000 * xs + ys)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  resA <- solveA parsed
  print resA
  resB <- solveB parsed
  print resB
  (resA, resB) @?= (543903, 14687245)

-- O1 : 14.0 s, repl : 37.0 s

-- strict map : -1.3 s : 12.7 , 33 s

-- vector - c : 0.4s , i : 43 s