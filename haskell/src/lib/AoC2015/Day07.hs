module AoC2015.Day07 where

import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Function.Memoize (Memoizable (memoize))
import qualified Data.Map as Map
import Data.Word (Word16)
import Test.HUnit ((@=?))
import Text.Read (readMaybe)

parse :: String -> Map.Map String Instr
parse = Map.fromList . fmap (f . words) . lines
 where
  f [x, "AND", y, "->", d] = (d, And (toSI x) (toSI y))
  f [x, "OR", y, "->", d] = (d, Or (toSI x) (toSI y))
  f [x, "LSHIFT", i, "->", d] = (d, LShift (toSI x) (read i))
  f [x, "RSHIFT", i, "->", d] = (d, RShift (toSI x) (read i))
  f ["NOT", v, "->", x] = (x, Not $ toSI v)
  f ([v, "->", x]) = (x, Val $ toSI v)
  f x = error $ show x

toSI :: String -> SI
toSI s = case readMaybe @Word16 s of
  Nothing -> Right s
  Just i -> Left i

solve :: Map.Map String Instr -> Word16
solve m = mem "a"
 where
  mem = memoize f

  f :: String -> Word16
  f target = case Map.lookup (id target) m of
    Just (And a b) -> ff a .&. ff b
    Just (Or a b) -> ff a .|. ff b
    Just (LShift a b) -> shiftL (ff a) b
    Just (RShift a b) -> shiftR (ff a) b
    Just (Not a) -> complement $ ff a
    Just (Val a) -> ff a
    Nothing -> error (show target)

  ff (Left w) = w
  ff (Right s) = mem s

type SI = Either Word16 String

data Instr
  = And SI SI
  | Or SI SI
  | Not SI
  | LShift SI Int
  | RShift SI Int
  | Val SI
  deriving (Show, Eq, Ord)

run :: String -> IO (String, String)
run xs = do
  let parsed = parse $ const xs "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"
  let resA = solve parsed
  print resA
  resA @=? 16076
  let reset = Map.insert "b" (Val $ Left resA) parsed
  let resB = solve reset
  print resB
  resB @=? 2797
  return mempty