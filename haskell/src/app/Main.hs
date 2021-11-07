module Main (
  main,
) where

import qualified AoC2020.Solutions as AoC2020
import CmdArgs
import Control.Lens
import Control.Monad (join, void)
import Data.Bitraversable (Bitraversable (bitraverse))
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import DayVersion
import Input (getInput)
import Options.Applicative (execParser)
import System.TimeIt
import Utils ((=:))

lastDayNr :: DayVersion
lastDayRunnner :: String -> IO (String, String)
(lastDayNr, lastDayRunnner) = Map.findMax AoC2020.solutions

runner :: Options -> IO ()
runner Options{day, input, year} = do
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i >>= void . bitraverse print print
        SpecificDay d ->
          case Map.lookup d AoC2020.solutions of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> Map.keys AoC2020.solutions)
            Just dayRunner ->
              dayRunner i >>= traverseOf_ both putStrLn
  inputFile <- case input of
    StdIn -> do
      getContents
    File path -> do
      readFile path
    Test -> do
      let path = "inputs/" <> show year <> "/" <> show lastDayNr <> "test"
      readFile path
    DayInput -> do
      case day of
        LastDay -> getInput year (getDayNum lastDayNr)
        SpecificDay d -> getInput year $ getDayNum d
  timeIt $ func inputFile

main :: IO ()
main = do
  let parser = cmdParser lastDayNr
  execParser parser >>= void . runner