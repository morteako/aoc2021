module AoC2020.Day18TH where

import Data.Either (fromRight)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH (Exp (ListE), Q, runIO)

e :: Q Exp
e = runIO $ do
  inp <- lines <$> readFile "../inputs/2020/18"
  pure $ ListE $ fromRight undefined . parseExp <$> inp