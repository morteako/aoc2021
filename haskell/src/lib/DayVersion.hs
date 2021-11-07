module DayVersion where

import Data.Char
import Data.String
import Data.String (IsString)
import Text.Read (readMaybe)

data DayVersion = SpecialVersion Int String | NormalDay Int deriving (Eq, Ord)

instance Show DayVersion where
  show (SpecialVersion n str) = show n <> str
  show (NormalDay n) = show n

getDayNum :: DayVersion -> Int
getDayNum (SpecialVersion n _) = n
getDayNum (NormalDay n) = n

instance IsString DayVersion where
  fromString s = case (readMaybe i, ss) of
    (Just n, "") -> NormalDay $ n
    (Just n, _) -> SpecialVersion n ss
    (Nothing, _) -> error $ s <> " is not a valid dayversion. Format : dd?label?"
   where
    (i, ss) = span isDigit s