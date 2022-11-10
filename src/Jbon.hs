module Jbon (JbonObject (..), getObjectDefinitions, tryGetIndexedSubList, minify, encode) where

import Core (Indexed, firstJust, indexed)
import Data.ByteString.Builder qualified as BSB (Builder, string8, stringUtf8, word32LE, word64LE, word8)
import Data.List (find, genericLength, nub, sortOn)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Json (JsonNumber (..), JsonValue (..))

{- | Definition of a Jbon object:
 - From which object index it inherits.
 - Which fields are present this object, besides the ones inherited.
 - All fields present in this object.
-}
data JbonObject = JbonObject (Maybe Word32) (Indexed String) [String] deriving (Eq, Show)

encode :: JsonValue -> BSB.Builder
encode value = BSB.string8 "JBON" <> header <> encode' value
 where
  objs = getObjectDefinitions value

  encodeLength :: forall a. [a] -> BSB.Builder
  encodeLength = BSB.word32LE . genericLength

  encodeStr :: String -> BSB.Builder
  encodeStr str = encodeLength str <> BSB.stringUtf8 str

  header :: BSB.Builder
  header = encodeLength objs <> mconcat (hEncode . snd <$> objs)

  hEncode :: JbonObject -> BSB.Builder
  hEncode (JbonObject inherit fields _) =
    maybe (BSB.word32LE 0) BSB.word32LE inherit <> encodeLength fields <> mconcat (encodeField <$> fields)

  encodeField :: (Word32, String) -> BSB.Builder
  encodeField (idx, str) = BSB.word32LE idx <> encodeStr str

  encode' :: JsonValue -> BSB.Builder
  encode' JsonNull = BSB.word8 0
  encode' (JsonBool False) = BSB.word8 1
  encode' (JsonBool True) = BSB.word8 2
  encode' (JsonNum False (JsonInt i)) = BSB.word8 3 <> BSB.word64LE i
  encode' (JsonNum True (JsonInt i)) = BSB.word8 4 <> BSB.word64LE i
  encode' (JsonNum False (JsonDecimal i d)) = BSB.word8 5 <> BSB.word64LE i <> BSB.word64LE d
  encode' (JsonNum True (JsonDecimal i d)) = BSB.word8 6 <> BSB.word64LE i <> BSB.word64LE d
  encode' (JsonStr str) = BSB.word8 7 <> encodeStr str
  encode' (JsonArr arr) = BSB.word8 8 <> encodeLength arr <> mconcat (encode' <$> arr)
  encode' (JsonObj fields) = BSB.word8 9 <> BSB.word32LE (getObjectId fields) <> mconcat (encode' . snd <$> fields)

  getObjectId :: forall a. [(String, a)] -> Word32
  getObjectId fields =
    fromJust $ fst <$> find (\(_, JbonObject _ _ objFields) -> (fst <$> fields) == objFields) objs

getObjectDefinitions :: JsonValue -> [(Word32, JbonObject)]
getObjectDefinitions = minify . extract
 where
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

  -- Given a list of property lists, ordered by the number of properties descending, makes JbonObjects.
  -- Starts at the biggest list of properties, and will always try to inherit from the biggest
  -- list of properties to its right in the list.
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
