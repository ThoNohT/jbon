module Jbon (JbonObject (..), getObjectDefinitions, tryGetIndexedSubList, minify, encode) where

import Core (Indexed, firstJust, indexed, safeMaximum)
import Data.Bits (shift)
import Data.ByteString.Builder qualified as BSB (Builder, string8, stringUtf8, word16LE, word32LE, word64LE, word8)
import Data.List (find, genericLength, nub, sortOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word16, Word32, Word64, Word8)
import Json (JsonNumber (..), JsonValue (..), maxArrayLength, maxDecimal, maxInt, maxStringLength)

{- | Definition of a Jbon object:
 - From which object index it inherits.
 - Which fields are present this object, besides the ones inherited.
 - All fields present in this object.
-}
data JbonObject = JbonObject (Maybe Word64) (Indexed String) [String] deriving (Eq, Show)

-- | Returns the number of fields in this object.
nFields :: JbonObject -> Word64
nFields (JbonObject _ _ fields) = genericLength fields

-- | Determines the length of the longest field name.
maxFieldLength :: JbonObject -> Word64
maxFieldLength (JbonObject _ _ fields) = safeMaximum $ genericLength <$> fields

-- | The number of bits to use for specific types of numbers in the encoded Jbon data
data WordSize = W8 | W16 | W32 | W64

-- | Determines the word size in which a value fits.
wordSize :: Word64 -> WordSize
wordSize w
  | w > fromIntegral (maxBound @Word32) = W64
  | w > fromIntegral (maxBound @Word16) = W32
  | w > fromIntegral (maxBound @Word8) = W16
  | otherwise = W8

-- | Converts a WordSize to a Word16 with the two least significant bits set to the corresponding value.
wordSizeBits :: WordSize -> Word16
wordSizeBits W8 = 0
wordSizeBits W16 = 1
wordSizeBits W32 = 2
wordSizeBits W64 = 3

-- | Settings to use when encoding or decoding Jbon, these are encoded in the first two bytes after the Jbon header.
data EncodingSettings = EncodingSettings
  { numberOfObjects :: WordSize
  , numberOfFields :: WordSize
  , arrayLength :: WordSize
  , stringLength :: WordSize
  , objectNameLength :: WordSize
  , intSize :: WordSize
  , decimalSize :: WordSize
  }

encode :: JsonValue -> BSB.Builder
encode value = BSB.string8 "JBON" <> settingsHeader <> objsHeader <> encode' value
 where
  objs = getObjectDefinitions value

  settings =
    EncodingSettings
      { numberOfObjects = wordSize $ genericLength objs
      , numberOfFields = wordSize $ safeMaximum $ nFields . snd <$> objs
      , arrayLength = wordSize $ maxArrayLength value
      , stringLength = wordSize $ maxStringLength value
      , objectNameLength = wordSize $ safeMaximum $ maxFieldLength . snd <$> objs
      , intSize = wordSize $ maxInt value
      , decimalSize = wordSize $ maxDecimal value
      }

  settingsHeader =
    BSB.word16LE $
      wordSizeBits (numberOfObjects settings)
        + shift (wordSizeBits $ numberOfFields settings) 2
        + shift (wordSizeBits $ arrayLength settings) 4
        + shift (wordSizeBits $ stringLength settings) 6
        + shift (wordSizeBits $ objectNameLength settings) 8

  encodeNumber :: WordSize -> Word64 -> BSB.Builder
  encodeNumber W8 = BSB.word8 . fromIntegral
  encodeNumber W16 = BSB.word16LE . fromIntegral
  encodeNumber W32 = BSB.word32LE . fromIntegral
  encodeNumber W64 = BSB.word64LE

  encodeLength :: forall a. WordSize -> [a] -> BSB.Builder
  encodeLength ws = encodeNumber ws . genericLength

  encodeStr :: String -> BSB.Builder
  encodeStr str = encodeLength (stringLength settings) str <> BSB.stringUtf8 str

  objsHeader :: BSB.Builder
  objsHeader = encodeLength (numberOfObjects settings) objs <> mconcat (hEncode . snd <$> objs)

  hEncode :: JbonObject -> BSB.Builder
  hEncode (JbonObject inherit fields _) =
    encodeNumber (numberOfObjects settings) (fromMaybe 0 inherit)
      <> encodeLength (numberOfFields settings) fields
      <> mconcat (encodeField <$> fields)

  encodeField :: (Word64, String) -> BSB.Builder
  encodeField (idx, str) = encodeNumber (numberOfFields settings) idx <> encodeStr str

  encode' :: JsonValue -> BSB.Builder
  encode' JsonNull = BSB.word8 0
  encode' (JsonBool False) = BSB.word8 1
  encode' (JsonBool True) = BSB.word8 2
  encode' (JsonNum False (JsonInt i)) = BSB.word8 3 <> encodeNumber (intSize settings) i
  encode' (JsonNum True (JsonInt i)) = BSB.word8 4 <> encodeNumber (intSize settings) i
  encode' (JsonNum False (JsonDecimal i d)) =
    BSB.word8 5 <> encodeNumber (intSize settings) i
      <> encodeNumber (decimalSize settings) d
  encode' (JsonNum True (JsonDecimal i d)) =
    BSB.word8 6 <> encodeNumber (intSize settings) i
      <> encodeNumber (decimalSize settings) d
  encode' (JsonStr str) = BSB.word8 7 <> encodeStr str
  encode' (JsonArr arr) = BSB.word8 8 <> encodeLength (arrayLength settings) arr <> mconcat (encode' <$> arr)
  encode' (JsonObj fields) =
    BSB.word8 9
      <> encodeNumber (numberOfObjects settings) (getObjectId fields)
      <> mconcat (encode' . snd <$> fields)

  getObjectId :: forall a. [(String, a)] -> Word64
  getObjectId fields =
    fromJust $ fst <$> find (\(_, JbonObject _ _ objFields) -> (fst <$> fields) == objFields) objs

getObjectDefinitions :: JsonValue -> [(Word64, JbonObject)]
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
