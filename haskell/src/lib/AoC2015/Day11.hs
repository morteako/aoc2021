module AoC2015.Day11 where

import Control.Lens
import Data.Map (Map)
import Data.Map as Map
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as V
import GHC.Exts (RealWorld)
import Test.HUnit ((@=?))

parse :: V.Unbox a => [a] -> V.Vector a
parse (init -> s) = V.generate (length s) (s !!)

type My = V.MVector RealWorld Char

solve :: My -> My
solve v = v
 where
  l = M.length v

next :: Num a => a -> Char -> (a, Char)
next i 'h' = (i, 'j')
next i 'n' = (i, 'p')
next i 'k' = (i, 'm')
next i 'z' = (i -1, 'a')
next i c = (i, succ c)

run :: String -> IO ()
run xs = do
  parsed <- V.thaw $ parse xs
  resA <- V.freeze $ solve parsed
  print resA
  let resB = solve parsed

  print "todo"

-- 0.00s