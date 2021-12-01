module AoC2020.Solutions where

import qualified AoC2020.Day01
import qualified AoC2020.Day02
import qualified AoC2020.Day03
import qualified AoC2020.Day04
import qualified AoC2020.Day05
import qualified AoC2020.Day06
import qualified AoC2020.Day07
import qualified AoC2020.Day08
import qualified AoC2020.Day09
import qualified AoC2020.Day10
import qualified AoC2020.Day11
import qualified AoC2020.Day12
import qualified AoC2020.Day13
import qualified AoC2020.Day14
import qualified AoC2020.Day15
import qualified AoC2020.Day16
import qualified AoC2020.Day17
import qualified AoC2020.Day18
import qualified AoC2020.Day19

-- import qualified AoC2020.Day20
import qualified AoC2020.Day21
import qualified AoC2020.Day22
import qualified AoC2020.Day23

-- import qualified AoC2020.Day24
import Data.Map (Map)
import qualified Data.Map as Map
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ --"1" =: Day01.run
      --, "1unboxed" =: Day01.run
      "01" =: AoC2020.Day01.run
    , "02" =: AoC2020.Day02.run
    , "03" =: AoC2020.Day03.run
    , "04" =: AoC2020.Day04.run
    , "05" =: AoC2020.Day05.run
    , "06" =: AoC2020.Day06.run
    , "07" =: AoC2020.Day07.run
    , "08" =: AoC2020.Day08.run
    , "09" =: AoC2020.Day09.run
    , "10" =: AoC2020.Day10.run
    , "11" =: AoC2020.Day11.run
    , "12" =: AoC2020.Day12.run
    , "13" =: AoC2020.Day13.run
    , "14" =: AoC2020.Day14.run
    , "15" =: AoC2020.Day15.run
    , "16" =: AoC2020.Day16.run
    , "17" =: AoC2020.Day17.run
    , "18" =: AoC2020.Day18.run
    , "19" =: AoC2020.Day19.run
    , "21" =: AoC2020.Day21.run
    , "22" =: AoC2020.Day22.run
    ]
