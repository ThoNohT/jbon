module Core (Indexed, firstJust, indexed, safeMaximum) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Word (Word64)

-- | [(Word64)]
type Indexed a = [(Word64, a)]

-- | Returns the first element in the list for which the function returns Just.
firstJust :: forall a b. (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- | Adds indexes to a list, starting from the provided value.
indexed :: forall a. Word64 -> [a] -> [(Word64, a)]
indexed = go
 where
  go i (x : xs) = (i, x) : go (i + 1) xs
  go _ _ = []

-- | A maximum that works with bounded numbers such that on an empty foldable, the minimum bound can be used.
safeMaximum :: forall t a. (Foldable t, Ord a, Bounded a) => t a -> a
safeMaximum f = if null f then minBound else maximum f
