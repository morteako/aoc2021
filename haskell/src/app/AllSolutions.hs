module AllSolutions where

import qualified AoC2015.Solutions as AoC2015
import qualified AoC2020.Solutions as AoC2020
import qualified AoC2021.Solutions as AoC2021
import CmdArgs (Year (..))
import Data.Map
import qualified Data.Map as Map
import DayVersion

getSolutionsForYear :: Year -> Map DayVersion (String -> IO ())
getSolutionsForYear Y2021 = AoC2021.solutions
getSolutionsForYear Y2020 = AoC2020.solutions
getSolutionsForYear Y2019 = Map.empty
getSolutionsForYear Y2018 = Map.empty
getSolutionsForYear Y2017 = Map.empty
getSolutionsForYear Y2016 = Map.empty
getSolutionsForYear Y2015 = AoC2015.solutions
