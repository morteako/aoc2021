module AoC2020.Day08 where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

data Instr = Acc Int | Jmp Int | Nop Int deriving (Show, Read)

parse :: String -> Map Int Instr
parse = Map.fromList . zip [0 ..] . fmap (read . filter (/= '+') . upperFirst) . lines
 where
  upperFirst = over (ix 0) toUpper

type S a = StateT (Sum Int, Set Int) (Reader (Map Int Instr)) a

pattern Loop = True
pattern Terminate = False

execInstr :: Int -> S Bool
execInstr i = do
  (_, visited) <- get
  if (Set.member i visited)
    then pure Loop
    else do
      _2 %= Set.insert i
      instr <- asks (Map.lookup i)
      case instr of
        Nothing -> pure Terminate
        Just (Acc a) -> do
          _1 <>= Sum a
          execInstr (succ i)
        Just (Nop _) -> execInstr (succ i)
        Just (Jmp j) -> execInstr (i + j)

solve :: Map Int Instr -> Sum Int
solve dict = view (_2 . _1) $ runInstrs dict

-- runInstrs :: Map Int Instr -> Int
-- runInstrs :: Map Int Instr -> (Bool, (Int, Set Int))
runInstrs dict = flip runReader dict $ runStateT (execInstr 0) mempty

flipInstr' :: Instr -> Maybe Instr
flipInstr' (Jmp i) = Just $ Nop i
flipInstr' (Nop i) = Just $ Jmp i
flipInstr' _ = Nothing

-- (Map.alterF) (fmap flipInstr') i dict

-- solve2 :: Map Int Instr -> [Int]
solve2 dict = do
  i <- Map.keys dict
  case runInstrs <$> failover (ix i . prism' id flipInstr') id dict of
    Nothing -> []
    Just (Loop, _) -> []
    Just (Terminate, (n, _)) -> pure n

run xs = do
  let parsed = parse xs

  print $ solve parsed
  print $ solve2 parsed
