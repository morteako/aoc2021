{-# LANGUAGE TemplateHaskell #-}

module AoC2020.Day18 where

import AoC2020.Day18TH
import Language.Haskell.Meta
import Prelude as P

solve1 = sum $(e)
 where
  infixl 5 +
  (+) = (P.+)

  infixl 5 *
  (*) = (P.*)

solve2 = sum $(e)
 where
  infixl 7 +
  (+) = (P.+)

  infixl 6 *
  (*) = (P.*)

run :: a -> IO ()
run _ = do
  print $ solve1 -- 7293529867931
  print $ solve2 -- 60807587180737
