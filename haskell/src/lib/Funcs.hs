module Funcs where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Day.Day01
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

funcs :: Map DayVersion (String -> IO (String, String))
funcs =
    Map.fromList
        [ "1" =: Day.Day01.run
        , "1unboxed" =: Day.Day01.run
        ]