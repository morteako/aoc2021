module AoC2020.Day04 where

import Data.Char
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec (
  Parsec,
  char,
  letter,
  many1,
  parse,
  satisfy,
  spaces,
 )
import qualified Utils

mandKeys =
  Set.fromList
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    ]

parsePassword :: Parsec String () (Map String String)
parsePassword = Map.fromList <$> many1 kvPair
 where
  kvPair :: Parsec String () (String, String)
  kvPair = do
    key <- many1 letter
    char ':'
    spaces
    val <- many1 (satisfy (not . isSeparator))
    spaces
    pure (key, val)

parseInput :: String -> [Map String String]
parseInput xs = map parseKeys $ prep xs
 where
  prep = fmap unwords . splitOn [""] . lines

parseKeys :: String -> Map String String
parseKeys s = either (error . show) id $ parse parsePassword "" s

solve1 :: [Map String String] -> Int
solve1 = Utils.countP check
 where
  check :: Map String a -> Bool
  check = Set.isSubsetOf mandKeys . Map.keysSet

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

solve2 :: [Map String [Char]] -> Int
solve2 = Utils.countP check
 where
  check m = all f checks
   where
    f (k, f) = maybe False f $ Map.lookup k m

  checks =
    [ (,) "byr" byr
    , (,) "iyr" iyr
    , (,) "eyr" eyr
    , (,) "hgt" hgt
    , (,) "hcl" hcl
    , (,) "ecl" ecl
    , (,) "pid" pid
    ]

  byr = between "1920" "2002"
  iyr = between "2010" "2020"
  eyr = between "2020" "2030"
  hgt w
    | "cm" `isSuffixOf` w = between "150" "193" (take 3 w)
    | "in" `isSuffixOf` w = between "59" "76" (take 2 w)
    | otherwise = False

  hcl w = take 1 w == "#" && length w == 7 && all isAlphaNum (drop 1 w) --TODO
  ecl = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  pid w = length w == 9 && all isDigit w

run :: String -> IO ()
run xs = do
  let parsed = parseInput xs
  print $ solve1 parsed -- 204
  print $ solve2 parsed -- 179
