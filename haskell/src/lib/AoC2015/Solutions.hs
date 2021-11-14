module AoC2015.Solutions where

import qualified AoC2015.Day01 as Day01
import qualified AoC2015.Day02 as Day02
import qualified AoC2015.Day03 as Day03
import qualified AoC2015.Day04 as Day04
import qualified AoC2015.Day05 as Day05
import qualified AoC2015.Day06 as Day06
import qualified AoC2015.Day07 as Day07
import qualified AoC2015.Day08 as Day08
import qualified AoC2015.Day09 as Day09
import qualified AoC2015.Day10 as Day10
import qualified AoC2015.Day11 as Day11
import qualified AoC2015.Day12 as Day12
import qualified AoC2015.Day13 as Day13
import qualified AoC2015.Day14 as Day14
import Data.Map (Map)
import qualified Data.Map as Map
import DayVersion (DayVersion)
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
    Map.fromList
        [ "1" =: Day01.run
        , "2" =: Day02.run
        , "3" =: Day03.run
        , "4" =: Day04.run
        , "5" =: Day05.run
        , "6" =: Day06.run
        , "7" =: Day07.run
        , "8" =: Day08.run
        , "9" =: Day09.run
        , "10" =: Day10.run
        , "11" =: Day11.run
        , "12" =: Day12.run
        , "13" =: Day13.run
        , "14" =: Day14.run
        ]