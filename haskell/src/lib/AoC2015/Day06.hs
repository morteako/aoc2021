{-# LANGUAGE MagicHash #-}

module AoC2015.Day06 where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Data.List (foldl', stripPrefix, uncons)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Dual (Dual, getDual), Endo (Endo, appEndo))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector, MVector, STVector, forM_)
import qualified Data.Vector.Mutable as V
import Linear (V2 (..))
import Test.HUnit ((@?=))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, digitChar, space, string)

solve :: [Commands] -> (Int, Int)
solve = (Map.size . Map.filter fst &&& sum . fmap snd) . foldl' (flip makeMap2) startMap

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

startMap :: Map (V2 Int) (Bool, Int)
startMap = pointMap (V2 0 0) (V2 999 999) (False, 0)

makeMap2 :: Commands -> Map (V2 Int) (Bool, Int) -> Map (V2 Int) (Bool, Int)
makeMap2 (Command TurnOn a b) w = Map.unionWith (mm (flip const) (+)) w (pointMap a b (True, 1))
makeMap2 (Command TurnOff a b) w = Map.unionWith (mm (flip const) monus) w (pointMap a b (False, 1))
makeMap2 (Command Toggle a b) w = Map.unionWith (mm (const . not) (+)) w (pointMap a b (False, 2))

monus :: (Ord a, Num a) => a -> a -> a
monus a b = max 0 (a - b)

mm f g (a, b) (c, d) = (f a c, g b d)

pointMap (V2 x y) (V2 xx yy) b = Map.fromList $ do
  xs <- [x .. xx]
  ys <- [y .. yy]
  pure (V2 xs ys, b)

run :: String -> IO (String, String)
run xs = do
  let parsed = parse xs
  let res = solve parsed
  print res
  res @?= (543903, 14687245)
  return mempty

-- O1 : 14.0 s, repl : 37.0 s

-- strict map : -1.3 s : 12.7 , 33 s