{-# LANGUAGE BangPatterns #-}

module AoC2015.Day19 where

import Control.Lens hiding (indices)
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Search
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashSet as Set
import Data.List.Split (splitOn)
import Debug.Trace
import Test.HUnit ((@=?))

parse = over _2 BSU.fromString . over _1 replecements . toTup . splitOn "\n\n"
 where
  replecements = over (each . each) BSU.fromString . fmap (toTup . splitOn " => ") . lines
  toTup [x, y] = (x, y)
  toTup xs = error $ show xs

solve (repls, start) = Set.size $ go (Set.singleton start)
 where
  go !olds =
    olds <> (Set.unions $ fmap (\r -> foldMap (\bs -> getAll bs r) $ olds) repls)

solveB (repls, target) = go 0 (Set.singleton "e")
 where
  checkDone = Set.member target
  go 10 _ = 10
  go n !olds | traceShow olds True && checkDone olds = n
  go n !olds =
    go (n + 1) (olds <> (Set.unions $ fmap (\r -> foldMap (\bs -> getAll bs r) $ olds) repls))

getAll bs (pat, repl) = Set.fromList $ fmap (\i -> insertAt i bs (pat, repl)) $ indices pat bs

insertAt i bs (pat, repl) = let (pre, post) = BS.splitAt i bs in pre <> repl <> BS.drop (BS.length pat) post

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  let resA = solve parsed
  print resA

  let resB = solveB parsed
  print resB

-- 0.00s