module Utils where

import Data.Foldable (Foldable (foldl'))
import Data.List.Extra hiding (foldl1')
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Debug.Trace
import GHC.Base (Semigroup)
import Linear (V2 (V2))

readInt :: String -> Int
readInt = read

(?:) :: Maybe c -> c -> c
(?:) = flip fromMaybe

(=:) = (,)

count :: Eq a => a -> [a] -> Int
count x = getSum . foldMap (Sum . fromEnum . (== x))

countP :: Foldable f => (a -> Bool) -> f a -> Int
countP p = getSum . foldMap (Sum . fromEnum . p)

newtype MIntersect k v = MIntersect (Map.Map k v)

instance (Ord k, Semigroup v) => Semigroup (MIntersect k v) where
    MIntersect a <> MIntersect b = MIntersect $ Map.intersectionWith (<>) a b

semiFoldMapl' :: (Semigroup v, Functor f, Foldable f) => (a -> v) -> f a -> v
semiFoldMapl' av = foldl1' (<>) . fmap av

semiFoldMapr :: (Semigroup v, Functor f, Foldable f) => (a -> v) -> f a -> v
semiFoldMapr av = foldr1 (<>) . fmap av

foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' f xs =
    fromMaybe
        (errorWithoutStackTrace "foldl1': empty structure")
        (foldl' mf Nothing xs)
  where
    mf m y =
        Just
            ( case m of
                Nothing -> y
                Just x -> f x y
            )

printMap m = do
    putStrLn "--------"
    let xs = Map.toList m
    let g = groupOn (\(V2 x _, _) -> x) xs
    let gg = fmap (fmap snd) g
    mapM_ print gg
    putStrLn ""

traceLab s x = trace (s ++ ": " ++ show x) x