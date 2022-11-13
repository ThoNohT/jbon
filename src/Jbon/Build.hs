module Jbon.Build (getObjectDefinitions, tryGetIndexedSubList, minify) where

import Core (firstJust)
import Data.List (nub, sortOn)
import Data.Word (Word64)
import Indexed (Indexed, indexed)
import Jbon.Jbon (JbonObject (..))
import Json (JsonValue (..))

-- | Creates jbon object definitions for all objects in the provided json value.
getObjectDefinitions :: JsonValue -> [(Word64, JbonObject)]
getObjectDefinitions = minify . extract

-- | Extracts all lists of fields from the objects contained in the provided json value.
extract :: JsonValue -> [[String]]
extract JsonNull = []
extract (JsonBool _) = []
extract (JsonNum _ _) = []
extract (JsonStr _) = []
extract (JsonArr elems) = concat $ extract <$> elems
extract (JsonObj values) = (fst <$> values) : concat (extract <$> fmap snd values)

{- | Given a list of property lists, converts a minimal list of JbonObjects, where each object tries to inherit from
 | another object such that it minimizes its own introduced properties.
-}
minify :: [[String]] -> Indexed JbonObject
minify definitions = reverse $ makeOptimalObjects [] ordered
 where
  ordered = reverse $ indexed 1 $ indexed 1 <$> nub (sortOn length definitions)

{- | Given a list of property lists, ordered by the number of properties descending, makes JbonObjects.
 | Starts at the biggest list of properties, and will always try to inherit from the biggest
 | list of properties to its right in the list.
-}
makeOptimalObjects :: Indexed JbonObject -> Indexed (Indexed String) -> Indexed JbonObject
makeOptimalObjects acc = \case
  [] -> reverse acc
  ((i, x) : xs) -> case firstJust (\(j, l2) -> (j,) <$> tryGetIndexedSubList l2 x) xs of
    Nothing -> makeOptimalObjects ((i, JbonObject Nothing x (snd <$> x)) : acc) xs
    Just (j, subList) -> makeOptimalObjects ((i, JbonObject (Just j) subList (snd <$> x)) : acc) xs

{- | Attempts to specify the second indexed list as an extension of the first indexed list.
 | The list of indexes will be the places where the fields should be inserted. If multiple entries with the same
 | index exist, they need to be inserted at that index (from the original list), in order.
-}
tryGetIndexedSubList :: Indexed String -> Indexed String -> Maybe (Indexed String)
tryGetIndexedSubList [] _ = Nothing -- Exending from an empty object is not helpful.
tryGetIndexedSubList l1 l2 = reverse <$> go [] 0 (sortOn fst l1) (sortOn fst l2)
 where
  go acc insertAt as bs =
    case (as, bs) of
      -- Both are empty, and since we have not exited yet,
      -- we can determine that the entire sublist has been built in acc.
      ([], []) -> Just acc
      -- as is longer than bs, it is definitely not a sublist.
      (_, []) -> Nothing
      -- Both have at least one element.
      -- If it is equal, skip the element, and keep going.
      -- If it is not equal, count the element from bs as an insertion and keep going.
      ((xi, xn) : xs, (_, yn) : ys) ->
        if xn == yn
          then go acc xi xs ys
          else go ((insertAt, yn) : acc) insertAt as ys
      -- bs is longer than as, so we can simply append all elements.
      ([], ys) -> Just $ ((insertAt,) . snd <$> reverse ys) <> acc
