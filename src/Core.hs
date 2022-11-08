module Core (Indexed, firstJust, indexed) where

import Data.Maybe (listToMaybe, mapMaybe)

-- | [(Int, a)]
type Indexed a = [(Int, a)]

-- | Returns the first element in the list for which the function returns Just.
firstJust :: forall a b. (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- | Adds indexes to a list, starting from the provided value.
indexed :: forall a. Int -> [a] -> [(Int, a)]
indexed = go
 where
  go i (x : xs) = (i, x) : go (i + 1) xs
  go _ _ = []
