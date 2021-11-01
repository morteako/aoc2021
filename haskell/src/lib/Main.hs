module Main (
  main,
  funcs,
) where

import CmdArgs
import Control.Monad (join, void)
import Data.Bitraversable
import qualified Data.IntMap as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Day.Day01
import Input (getInput)
import Options.Applicative (execParser)
import Utils ((=:))

funcs :: IntMap (String -> IO (String, String))
funcs =
  Map.fromList
    [ 1 =: Day.Day01.run
    ]

lastDayNr :: Int
lastDayRunnner :: String -> IO (String, String)
(lastDayNr, lastDayRunnner) = IntMap.findMax funcs

runner :: Options -> IO ()
runner Options{day, input} = do
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i >>= void . bitraverse print print
        SpecificDay d ->
          case IntMap.lookup d funcs of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> IntMap.keys funcs)
            Just dayRunner ->
              dayRunner i >>= void . bitraverse print print
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
  execParser parser >>= void . runner
