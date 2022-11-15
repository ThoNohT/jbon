module Core (firstJust, safeMaximum, notMinBound, subtractNums) where

import Data.Map.Strict qualified as Map (Map, lookup)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

-- | Returns the first element in the list for which the function returns Just.
firstJust :: forall a b. (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- | A maximum that works with bounded numbers such that on an empty foldable, the minimum bound can be used.
safeMaximum :: forall t a. (Foldable t, Ord a, Bounded a) => t a -> a
safeMaximum f = if null f then minBound else maximum f

-- | Wraps a Bounded type in Maybe, where it is Nothing if it is equal to minBound.
notMinBound :: forall a. (Eq a, Bounded a) => a -> Maybe a
notMinBound x | x == minBound = Nothing
notMinBound other = Just other

{- | Subtract the values in the second map from the values in the first map. If a value becomes minBound,
 |  remove it from the map.
-}
subtractNums :: forall k n. (Ord k, Ord n, Num n, Bounded n) => [(k, n)] -> Map.Map k n -> [(k, n)]
subtractNums m1 m2 = mapMaybe subtractElem m1
 where
  subtractElem :: (k, n) -> Maybe (k, n)
  subtractElem (k, num1) =
    case num1 - fromMaybe minBound (Map.lookup k m2) of
      diff
        | diff > minBound -> Just (k, diff)
        | otherwise -> Nothing
