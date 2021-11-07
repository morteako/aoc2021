module Main (
  main,
) where

import CmdArgs
import Control.Lens
import Control.Monad (join, void)
import Data.Bitraversable (Bitraversable (bitraverse))
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import qualified Day.Day01
import DayVersion
import qualified Funcs
import Input (getInput)
import Options.Applicative (execParser)
import System.TimeIt
import Utils ((=:))

lastDayNr :: DayVersion
lastDayRunnner :: String -> IO (String, String)
(lastDayNr, lastDayRunnner) = Map.findMax Funcs.funcs

runner :: Options -> IO ()
runner Options{day, input} = do
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i >>= void . bitraverse print print
        SpecificDay d ->
          case Map.lookup d Funcs.funcs of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> Map.keys Funcs.funcs)
            Just dayRunner ->
              dayRunner i >>= traverseOf_ both putStrLn
  inputFile <- case input of
    StdIn -> do
      getContents
    File path -> do
      readFile path
    Test -> do
      let path = "input/" <> show lastDayNr <> "test"
      readFile path
    DayInput -> do
      case day of
        LastDay -> getInput (getDayNum lastDayNr)
        SpecificDay d -> getInput $ getDayNum d
  timeIt $ func inputFile

main :: IO ()
main = do
  let parser = cmdParser lastDayNr
  execParser parser >>= void . runner