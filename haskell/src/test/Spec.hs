module Spec where

import Data.Bitraversable (Bitraversable (bitraverse))
import Data.IntMap
import qualified Data.IntMap as Map
import qualified Data.List.Extra as Map
import qualified Day.Day01
import Input (getInput)
import Main (funcs)
import Test.Hspec (describe, hspec, it, runIO, shouldBe)
import Utils

answers :: IntMap (String, String)
answers =
  Map.fromList
    [ 1 =: ("802011", "248607374")
    ]

makePartString :: Int -> String
makePartString part = "Part " <> show part <> " is correct"

main :: IO ()
main = hspec $ do
  describe "" $ do
    let funcsAndAnswers = Map.intersectionWith (,) funcs answers
    flip Map.traverseWithKey funcsAndAnswers $ \k (f, (ansA, ansB)) -> do
      input <- runIO $ getInput k
      describe ("Day : " <> show k) $ do
        (actualA, actualB) <- runIO $ f input
        it (makePartString 1) $ do
          actualA `shouldBe` ansA
        it (makePartString 2) $ do
          actualB `shouldBe` ansB
    pure ()