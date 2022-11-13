module Core (firstJust, safeMaximum, notMinBound) where

import Data.Maybe (listToMaybe, mapMaybe)

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
