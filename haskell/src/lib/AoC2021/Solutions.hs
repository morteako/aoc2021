module AoC2021.Solutions where

import qualified AoC2021.Day01

import qualified AoC2021.Day02
import qualified AoC2021.Day03
import qualified AoC2021.Day04
import qualified AoC2021.Day06
import qualified AoC2021.Day07
import qualified AoC2021.Day08
import qualified AoC2021.Day09
import qualified AoC2021.Day10
import qualified AoC2021.Day11
import qualified AoC2021.Day12
import qualified AoC2021.Day13
import qualified AoC2021.Day14
import qualified AoC2021.Day15
import qualified AoC2021.Day16
import qualified AoC2021.Day17
import qualified AoC2021.Day18
import qualified AoC2021.Day20
import qualified AoC2021.Day22

import Data.Map (Map)
import qualified Data.Map as Map
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "01" =: AoC2021.Day01.run
    , "02" =: AoC2021.Day02.run
    , "03" =: AoC2021.Day03.run
    , "04" =: AoC2021.Day04.run
    , "06" =: AoC2021.Day06.run
    , "07" =: AoC2021.Day07.run
    , "08" =: AoC2021.Day08.run
    , "09" =: AoC2021.Day09.run
    , "10" =: AoC2021.Day10.run
    , "11" =: AoC2021.Day11.run
    , "12" =: AoC2021.Day12.run
    , "13" =: AoC2021.Day13.run
    , "14" =: AoC2021.Day14.run
    , "15" =: AoC2021.Day15.run
    , "16" =: AoC2021.Day16.run
    , "17" =: AoC2021.Day17.run
    , "18" =: AoC2021.Day18.run
    , -- , "19" =: AoC2021.Day19.run
      "20" =: AoC2021.Day20.run
    , -- , "21" =: AoC2021.Day21.run
      "22" =: AoC2021.Day22.run
    ]
