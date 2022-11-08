module Bson (BsonObject (..), BsonValue (..), getObjectDefinitions, tryGetIndexedSubList, minify) where

import Core (Indexed, firstJust, indexed)
import Data.List (nub, sortOn)
import Json (JsonValue (..))

{- | Definition of a Bson object:
 - From which object index it inherits.
 - Which fields are present this object, besides the ones inherited.
 - All fields present in this object.
-}
data BsonObject = BsonObject (Maybe Int) (Indexed String) [String] deriving (Eq, Show)

-- | Indication of what type of value comes next.
data BsonValue = BsonObj Int | BsonArr | BsonStr | BsonNum | BsonBool | BsonNull deriving (Eq, Show)

getObjectDefinitions :: JsonValue -> [(Int, BsonObject)]
getObjectDefinitions = minify . extract
 where
  extract :: JsonValue -> [[String]]
  extract JsonNull = []
  extract (JsonBool _) = []
  extract (JsonNum _ _) = []
  extract (JsonStr _) = []
  extract (JsonArr elems) = concat $ extract <$> elems
  extract (JsonObj values) = (fst <$> values) : concat (extract <$> fmap snd values)

{- | Given a list of property lists, converts a minimal list of BsonObjects, where each object tries to inherit from
 | another object such that it minimizes its own introduced properties.
-}
minify :: [[String]] -> Indexed BsonObject
minify definitions = makeOptimalObjects [] ordered
 where
  ordered = reverse $ indexed 1 $ indexed 1 <$> nub (sortOn length definitions)

  -- Given a list of property lists, ordered by the number of properties descending, makes BsonObjects.
  -- Starts at the biggest list of properties, and will always try to inherit from the biggest
  -- list of properties to its right in the list.
  makeOptimalObjects :: Indexed BsonObject -> Indexed (Indexed String) -> Indexed BsonObject
  makeOptimalObjects acc = \case
    [] -> reverse acc
    ((i, x) : xs) -> case firstJust (\(j, l2) -> (j,) <$> tryGetIndexedSubList l2 x) xs of
      Nothing -> makeOptimalObjects ((i, BsonObject Nothing x (snd <$> x)) : acc) xs
      Just (j, subList) -> makeOptimalObjects ((i, BsonObject (Just j) subList (snd <$> x)) : acc) xs

{- | Attempts to specify The first indexed list as an extension of the second indexed list.
 | The list of indexes will be the places where the fields should be inserted. If multiple entries with the same
 | index exist, they need to be inserted at that index (from the original list), in order.
-}
tryGetIndexedSubList :: Indexed String -> Indexed String -> Maybe (Indexed String)
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
