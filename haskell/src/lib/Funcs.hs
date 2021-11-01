module Funcs where

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import qualified Day.Day01
import Utils

funcs :: IntMap (String -> IO (String, String))
funcs =
    Map.fromList
        [ 1 =: Day.Day01.run
        ]