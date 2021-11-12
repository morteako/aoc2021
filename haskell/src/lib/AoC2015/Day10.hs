module AoC2015.Day10 where

import Data.Char (isDigit)
import Test.HUnit ((@=?))

parse :: [Char] -> [Int]
parse = map (read . pure) . filter isDigit

iter :: [Int] -> [Int]
iter s = do
  (c, i) <- fastGroup s
  [i, c]

-- a lot faster than using `Data.List.group`, mainly because it avoids `length` and building a string
fastGroup :: Eq a => [a] -> [(a, Int)]
fastGroup [] = []
fastGroup (x : xs) = go x 0 xs
 where
  go c i [] = [(c, i + 1)]
  go c i (s : xs) | c == s = go c (i + 1) xs
  go c i (s : xs) = (c, i + 1) : go s 0 xs

solve :: Int -> [Int] -> Int
solve n = length . (!! n) . iterate iter

-- solve 40 parsed ?> 492982 =>> print

run :: String -> IO ()
run xs = do
  print xs
  let parsed = parse $ const xs "111221"
  print $ fastGroup parsed
  let resA = solve 40 parsed
  print resA
  resA @=? 492982
  let resB = solve 50 parsed
  print resB
  resB @=? 6989950

--int : c: 0.36 s, i : 17.0s