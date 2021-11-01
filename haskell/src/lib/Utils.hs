module Utils where

import Data.Maybe (fromMaybe)

(?:) :: Maybe c -> c -> c
(?:) = flip fromMaybe

(=:) = (,)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

countP p = length . filter p