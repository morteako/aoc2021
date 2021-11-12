module AoC2015.Day08 where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Arrow (Arrow ((***)))
import Data.List (genericLength)
import Data.Semigroup (Sum (Sum))
import Test.HUnit ((@=?))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data Code = C Char | Esc ECode deriving (Show)

data ECode = X | Single deriving (Show)

parse :: String -> [(String, [Code])]
parse = fmap createStr . lines
 where
  createStr s = (s, either undefined id $ P.runParser str "" $ tail $ init $ s)
  str = many (esc <|> C <$> cha)
  esc = do
    P.char '\\'
    P.char 'x' *> cha *> cha *> pure (Esc X) <|> cha *> pure (Esc Single)

  cha :: P.Parsec String String Char
  cha = P.satisfy (const True)

solveA :: [(String, [Code])] -> Sum Int
solveA = abs . uncurry (-) . foldMap (Sum . length *** Sum . length)

solveB :: [([a], [Code])] -> Sum Int
solveB = abs . uncurry (-) . foldMap (Sum . length *** count2)

count2 :: Num a => [Code] -> Sum a
count2 code = 6 + foldMap @[] f code
 where
  f C{} = Sum 1
  f (Esc X) = Sum 5
  f (Esc Single) = Sum 4

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solveA parsed
  print resA
  resA @=? 1342
  let resB = solveB parsed
  print resB
  resB @=? 2074

-- 0.00s