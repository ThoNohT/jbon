module Indexed (Indexed, index, indexed) where

import Data.List qualified as L (find)
import Data.Word (Word64)

-- | [(Word64, a)]
type Indexed a = [(Word64, a)]

-- | Tries to find the element with the specified index in an indexed list.
index :: Word64 -> Indexed a -> Maybe a
index idx lst = snd <$> L.find (\(idx', _) -> idx == idx') lst

-- | Adds indexes to a list, starting from the provided value.
indexed :: forall a. Word64 -> [a] -> [(Word64, a)]
indexed = go
 where
  go i (x : xs) = (i, x) : go (i + 1) xs
  go _ _ = []
