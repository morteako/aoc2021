module AoC2020.Solutions where

import qualified AoC2020.Day01 as Day01
import Data.Map (Map)
import qualified Data.Map as Map
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO ())
solutions =
    Map.fromList
        [ "1" =: Day01.run
        , "1unboxed" =: Day01.run
        ]