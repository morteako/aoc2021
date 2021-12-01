module CmdArgs where

import qualified Data.Bifunctor as Bifunctor
import DayVersion
import Options.Applicative as Opt
import Text.Megaparsec as Parsec
import Text.Read (readMaybe)

data Year = Y2015 | Y2016 | Y2017 | Y2018 | Y2019 | Y2020 | Y2021 deriving (Show, Read)

getYear :: Year -> Int
getYear Y2015 = 2015
getYear Y2016 = 2016
getYear Y2017 = 2017
getYear Y2018 = 2018
getYear Y2019 = 2019
getYear Y2020 = 2020
getYear Y2021 = 2021

data Options = Options
  { day :: Day
  , input :: Input
  , year :: Year
  }
  deriving (Show)

data Day = LastDay | SpecificDay DayVersion deriving (Show)

data Input = StdIn | File String | Test | DayInput deriving (Show)

megaparsecReader :: Parsec String String a -> ReadM a
megaparsecReader parser =
  eitherReader (Bifunctor.first show . Parsec.parse parser "")

cmdParser :: ParserInfo Options
cmdParser =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc ("Run a advent of code challenge. Default is to run the last implemented challenge and fetch the corresponding input")
        <> header "aoc2021 - haskell solutions for advent of code 2021"
    )

options :: Parser Options
options =
  Options
    <$> (specificDayInput <|> pure LastDay)
    <*> (stdInput <|> fileInput <|> testInput <|> pure DayInput)
    <*> yearInput

yearInput :: Parser Year
yearInput =
  toYear
    <$> Opt.option
      auto
      ( long "year"
          <> metavar "YEAR"
          <> value 2021
          <> showDefault
          <> help "Chose which year of AoC"
      )
 where
  toYear :: Int -> Year
  toYear 2021 = Y2021
  toYear 2020 = Y2020
  toYear 2019 = Y2019
  toYear 2018 = Y2018
  toYear 2017 = Y2017
  toYear 2016 = Y2016
  toYear 2015 = Y2015
  toYear n = error $ "Only valid AoC years are 2015-2021 , not : " <> show n

specificDayInput :: Parser Day
specificDayInput =
  SpecificDay
    <$> strOption
      ( long "day"
          <> metavar "DAY"
          <> help "Run challenge for the provided day"
      )

fileInput :: Parser Input
fileInput =
  File
    <$> strOption
      ( long "file"
          <> metavar "FILENAME"
          <> help "Read from input file"
      )

stdInput :: Parser Input
stdInput =
  flag'
    StdIn
    ( long "stdin"
        <> help "Read from stdin"
    )

testInput :: Parser Input
testInput =
  flag'
    Test
    ( short 'T'
        <> long "test"
        <> help "Reads from 'input/DAYtest'"
    )
