module Bson (BsonObject (..), getObjectDefinitions, tryGetIndexedSubList, minify, encode) where

import Core (Indexed, firstJust, indexed)
import Data.List (find, nub, sortOn)
import Data.Maybe (fromJust)
import Json (JsonNumber (..), JsonValue (..))

{- | Definition of a Bson object:
 - From which object index it inherits.
 - Which fields are present this object, besides the ones inherited.
 - All fields present in this object.
-}
data BsonObject = BsonObject (Maybe Int) (Indexed String) [String] deriving (Eq, Show)

encode :: JsonValue -> [String]
encode value = "BSON" : header <> encode' value
 where
  objs = getObjectDefinitions value

  encodeStr :: String -> [String]
  encodeStr str = [show $ length str, str]

  header :: [String]
  header = show (length objs) : concat (hEncode . snd <$> objs)

  hEncode :: BsonObject -> [String]
  hEncode (BsonObject inherit fields _) = [maybe "0" show inherit, show $ length fields] <> concat (encodeField <$> fields)

  encodeField :: (Int, String) -> [String]
  encodeField (idx, str) = show idx : encodeStr str

  encode' :: JsonValue -> [String]
  encode' JsonNull = ["0"]
  encode' (JsonBool False) = ["1"]
  encode' (JsonBool True) = ["2"]
  encode' (JsonNum False (JsonInt i)) = ["3", show i]
  encode' (JsonNum True (JsonInt i)) = ["4", show i]
  encode' (JsonNum False (JsonDecimal i d)) = ["5", show i, show d]
  encode' (JsonNum True (JsonDecimal i d)) = ["6", show i, show d]
  encode' (JsonStr str) = "7" : encodeStr str
  encode' (JsonArr arr) = ["8", show $ length arr] <> concat (encode' <$> arr)
  encode' (JsonObj fields) = ["9", show $ getObjectId fields] <> concat (encode' . snd <$> fields)

  getObjectId :: forall a. [(String, a)] -> Int
  getObjectId fields =
    fromJust $ fst <$> find (\(_, BsonObject _ _ objFields) -> (fst <$> fields) == objFields) objs

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
minify definitions = reverse $ makeOptimalObjects [] ordered
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
