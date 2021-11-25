module AoC2015.Day20 where

import Control.Monad (when)
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  MonadIO (liftIO),
  runExceptT,
  when,
 )
import Data.Foldable (for_)
import qualified Data.Vector.Unboxed.Mutable as M
import Test.HUnit ((@=?))

parse = read @Int

solveA :: Int -> ExceptT Int IO ()
solveA lim = do
  houses <- M.replicate lim 0
  liftIO $ print "1"
  for_ [1 .. lim -1] $ \elf -> do
    for_ [elf, elf + elf .. lim -1] $ \e -> do
      M.modify houses (+ elf) e
    c <- M.read houses elf
    when (c >= lim) $ throwError elf

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  resA <- runExceptT $ solveA parsed
  print resA

-- 0.00s
