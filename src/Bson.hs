module Bson (BsonObject (..), BsonValue (..), indexed, tryGetIndexedSubList) where

import Data.List (sortOn)
import Json (JsonValue (..))

-- | Defines which fields are present in an object of this type.
data BsonObject = BsonObject {inheritFrom :: Maybe Int, objectFields :: [(Int, String)]} deriving (Eq, Show)

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

minify :: [[String]] -> [(Int, BsonObject)]
minify definitions = undefined
 where
  ordered = reverse $ indexed 1 $ indexed 1 <$> sortOn length definitions

{- | Attempts to specify The first indexed list as an extension of the second indexed list.
 | The list of indexes will be the places where the fields should be inserted. If multiple entries with the same
 | index exist, they need to be inserted at that index (from the original list), in order.
-}
tryGetIndexedSubList :: [(Int, String)] -> [(Int, String)] -> Maybe [(Int, String)]
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

-- | Adds indexes to a list, starting from the provided value.
indexed :: Int -> [a] -> [(Int, a)]
indexed = go
 where
  go i (x : xs) = (i, x) : go (i + 1) xs
  go _ _ = []
