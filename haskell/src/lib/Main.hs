module Main (
  main,
) where

import CmdArgs
import qualified Data.IntMap as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Day.Day01
import Input (getInput)
import Options.Applicative (execParser)
import Utils ((=:))

funcs :: IntMap (String -> IO ())
funcs =
  Map.fromList
    [ 1 =: Day.Day01.run
    ]

lastDayNr :: Int
lastDayRunnner :: String -> IO ()
(lastDayNr, lastDayRunnner) = IntMap.findMax funcs

runner :: Options -> IO ()
runner Options{day, input} = do
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i
        SpecificDay d ->
          case IntMap.lookup d funcs of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> IntMap.keys funcs)
            Just dayRunner ->
              dayRunner i
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
        LastDay -> getInput lastDayNr
        SpecificDay d -> getInput d
  func inputFile

main :: IO ()
main = do
  let parser = cmdParser lastDayNr
  execParser parser >>= runner
