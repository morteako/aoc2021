module AoC2015.Day07 where

import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Function.Memoize (Memoizable (memoize))
import qualified Data.Map as Map
import Data.Word (Word16)
import Test.HUnit ((@=?))
import Text.Read (readMaybe)

data Instr
  = And WS WS
  | Or WS WS
  | Not WS
  | LShift WS Int
  | RShift WS Int
  | Val WS
  deriving (Show)

parse :: String -> Map.Map String Instr
parse = Map.fromList . fmap (f . words) . lines
 where
  f [x, "AND", y, "->", d] = (d, And (toWS x) (toWS y))
  f [x, "OR", y, "->", d] = (d, Or (toWS x) (toWS y))
  f [x, "LSHIFT", i, "->", d] = (d, LShift (toWS x) (read i))
  f [x, "RSHIFT", i, "->", d] = (d, RShift (toWS x) (read i))
  f ["NOT", v, "->", x] = (x, Not $ toWS v)
  f ([v, "->", x]) = (x, Val $ toWS v)
  f x = error $ show x

type WS = Either Word16 String

toWS :: String -> WS
toWS s = case readMaybe @Word16 s of
  Nothing -> Right s
  Just i -> Left i

solve :: Map.Map String Instr -> Word16
solve m = mem "a"
 where
  mem = memoize eval

  eval :: String -> Word16
  eval target = case Map.lookup target m of
    Just (And a b) -> findValue a .&. findValue b
    Just (Or a b) -> findValue a .|. findValue b
    Just (LShift a b) -> shiftL (findValue a) b
    Just (RShift a b) -> shiftR (findValue a) b
    Just (Not a) -> complement $ findValue a
    Just (Val a) -> findValue a
    Nothing -> error (show target)

  findValue (Left w) = w
  findValue (Right s) = mem s

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solve parsed
  print resA
  resA @=? 16076
  let reset = Map.insert "b" (Val $ Left resA) parsed
  let resB = solve reset
  print resB
  resB @=? 2797

-- 0.00s