module AoC2015.Solutions where

import qualified AoC2015.Day01 as Day01
import qualified AoC2015.Day02 as Day02
import qualified AoC2015.Day03 as Day03
import qualified AoC2015.Day04 as Day04
import Data.Map (Map)
import qualified Data.Map as Map
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO (String, String))
solutions =
    Map.fromList
        [ "1" =: Day01.run
        , "2" =: Day02.run
        , "3" =: Day03.run
        , "4" =: Day04.run
        ]