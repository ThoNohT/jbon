module Jbon.Encode (
  EncodingSettings (..),
  WordSize (..),
  encodeJbon,
  settingsToWords,
  wordsToSettings,
  makeSettings,
  applyReferences,
  gatherDuplicates,
  countValues,
) where

import Core (safeMaximum, subtractNums)
import Data.Bifunctor (Bifunctor (second))
import Data.Bits (Bits ((.&.)), shift)
import Data.ByteString.Builder qualified as BSB (
  Builder,
  string8,
  stringUtf8,
  toLazyByteString,
  word16LE,
  word32LE,
  word64LE,
  word8,
 )
import Data.ByteString.Lazy qualified as BSL (unpack)
import Data.Foldable (Foldable (foldl'))
import Data.List (genericLength, sortOn, uncons)
import Data.Map.Strict qualified as Map (Map, alter, empty, filter, toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (Down))
import Data.Word (Word16, Word32, Word64, Word8)
import Indexed (Indexed)
import Jbon.Jbon (JbonObject (..), getObjectId, maxFieldLength, nFields)
import Json.Json (
  JsonExponent (..),
  JsonNumber (..),
  JsonValue (..),
  expIsNegative,
  isDecimal,
  maxArrayLength,
  maxDecimal,
  maxExponent,
  maxInt,
  maxStringLength,
  replaceValue,
 )

-- | Word size

-- | The number of bits to use for specific types of numbers in the encoded jbon data.
data WordSize = W8 | W16 | W32 | W64 deriving (Show)

-- | Determines the word size in which a value fits.
wordSize :: Word64 -> WordSize
wordSize w
  | w > fromIntegral (maxBound @Word32) = W64
  | w > fromIntegral (maxBound @Word16) = W32
  | w > fromIntegral (maxBound @Word8) = W16
  | otherwise = W8

-- | Converts a WordSize to a Word16 with the two least significant bits set to the corresponding value.
wordSizeBits :: forall a. Num a => WordSize -> a
wordSizeBits W8 = 0
wordSizeBits W16 = 1
wordSizeBits W32 = 2
wordSizeBits W64 = 3

-- | Converts a Word16 with the two least significant bits set to a word size, to the corresponding WordSize.
bitsWordSize :: forall a. (Num a, Bits a) => a -> Maybe WordSize
bitsWordSize 0 = Just W8
bitsWordSize 1 = Just W16
bitsWordSize 2 = Just W32
bitsWordSize 3 = Just W64
bitsWordSize _ = Nothing

-- | Converts a WordSize to the number of bytes it needs.
wordSizeNoBytes :: WordSize -> Word64
wordSizeNoBytes W8 = 1
wordSizeNoBytes W16 = 2
wordSizeNoBytes W32 = 4
wordSizeNoBytes W64 = 8

-- Encoding settings.

-- | Settings to use when encoding or decoding jbon, these are encoded in the first two bytes after the jbon header.
data EncodingSettings = EncodingSettings
  { numberOfObjects :: WordSize
  , numberOfFields :: WordSize
  , arrayLength :: WordSize
  , stringLength :: WordSize
  , objectNameLength :: WordSize
  , intSize :: WordSize
  , decimalSize :: WordSize
  , numberOfReferences :: WordSize
  , exponentSize :: WordSize
  }
  deriving (Show)

settingsToWords :: EncodingSettings -> (Word16, Word8)
settingsToWords settings =
  ( wordSizeBits (numberOfObjects settings)
      + shift (wordSizeBits $ numberOfFields settings) 2
      + shift (wordSizeBits $ arrayLength settings) 4
      + shift (wordSizeBits $ stringLength settings) 6
      + shift (wordSizeBits $ objectNameLength settings) 8
      + shift (wordSizeBits $ intSize settings) 10
      + shift (wordSizeBits $ decimalSize settings) 12
      + shift (wordSizeBits $ numberOfReferences settings) 14
  , wordSizeBits (exponentSize settings)
  )

wordsToSettings :: (Word16, Word8) -> Maybe EncodingSettings
wordsToSettings (msb, lsb) =
  EncodingSettings <$> bitsWordSize (msb .&. 3)
    <*> bitsWordSize (shift msb (-2) .&. 3)
    <*> bitsWordSize (shift msb (-4) .&. 3)
    <*> bitsWordSize (shift msb (-6) .&. 3)
    <*> bitsWordSize (shift msb (-8) .&. 3)
    <*> bitsWordSize (shift msb (-10) .&. 3)
    <*> bitsWordSize (shift msb (-12) .&. 3)
    <*> bitsWordSize (shift msb (-12) .&. 3)
    <*> bitsWordSize (lsb .&. 3)

-- | Creates encoding settings given a json value, and the calculated jbon objects.
makeSettings :: JsonValue -> Indexed JbonObject -> EncodingSettings
makeSettings value objs =
  EncodingSettings
    { numberOfObjects = wordSize $ genericLength objs
    , numberOfFields = wordSize $ safeMaximum $ nFields . snd <$> objs
    , arrayLength = wordSize $ maxArrayLength value
    , stringLength = wordSize $ maxStringLength value
    , objectNameLength = wordSize $ safeMaximum $ maxFieldLength . snd <$> objs
    , intSize = wordSize $ maxInt value
    , decimalSize = wordSize $ maxDecimal value
    , numberOfReferences = W8 -- Filled in later.
    , exponentSize = wordSize $ maxExponent value
    }

-- General encoders

-- | Encodes a number using the provided word size.
encodeNumber :: WordSize -> Word64 -> BSB.Builder
encodeNumber W8 = BSB.word8 . fromIntegral
encodeNumber W16 = BSB.word16LE . fromIntegral
encodeNumber W32 = BSB.word32LE . fromIntegral
encodeNumber W64 = BSB.word64LE

-- | Encodes a list length using the provided word size.
encodeLength :: forall a. WordSize -> [a] -> BSB.Builder
encodeLength ws = encodeNumber ws . genericLength

{- | Encodes a string, first specifies the length of the string using the provided word size,
 | and then UTF-8 encodes the string.
-}
encodeStr :: WordSize -> String -> BSB.Builder
encodeStr stringLength str =
  let encStr = BSB.stringUtf8 str
   in encodeLength stringLength (BSL.unpack $ BSB.toLazyByteString encStr) <> encStr

-- Jbon related encoders

-- | Gathers references and updates the settings and value with these references.
applyReferences :: EncodingSettings -> JsonValue -> (EncodingSettings, Indexed JsonValue, JsonValue)
applyReferences settings value =
  let (value', refs) = gatherDuplicates settings value
      settings' = settings{numberOfReferences = wordSize $ genericLength refs}
   in (settings', refs, value')

-- | Encodes a json value to jbon.
encodeJbon :: Indexed JbonObject -> JsonValue -> BSB.Builder
encodeJbon objs value' =
  BSB.string8 "JBON" <> settingsHeader <> objsHeader <> refsHeader <> body
 where
  settings' = makeSettings value' objs
  (settings, refs, value) = applyReferences settings' value'

  settingsHeader :: BSB.Builder
  settingsHeader =
    let (msb, lsb) = settingsToWords settings
     in BSB.word16LE msb <> BSB.word8 lsb

  objsHeader :: BSB.Builder
  objsHeader =
    encodeLength (numberOfObjects settings) objs
      <> mconcat (encodeJbonObject settings . snd <$> objs)

  refsHeader :: BSB.Builder
  refsHeader =
    encodeLength (numberOfReferences settings) refs
      <> mconcat (encodeReference <$> refs)

  encodeReference :: (Word64, JsonValue) -> BSB.Builder
  encodeReference (idx, v) =
    encodeNumber (numberOfReferences settings) idx
      <> encodeValue settings objs v

  body :: BSB.Builder
  body = encodeValue settings objs value

-- | Encodes a single jbon object using the provided encoding settings.
encodeJbonObject :: EncodingSettings -> JbonObject -> BSB.Builder
encodeJbonObject settings (JbonObject inherit fields _) =
  encodeNumber (numberOfObjects settings) (fromMaybe minBound inherit)
    <> encodeLength (numberOfFields settings) fields
    <> mconcat (encodeField settings <$> fields)

-- | Encodes a single field using the provided encoding settings.
encodeField :: EncodingSettings -> (Word64, String) -> BSB.Builder
encodeField settings (idx, str) =
  encodeNumber (numberOfFields settings) idx <> encodeStr (stringLength settings) str

-- | Determines the number representing the value type.
valueTypeNumber :: JsonValue -> BSB.Builder
valueTypeNumber val = BSB.word8 $
  case val of
    JsonNull -> 0
    JsonBool False -> 1
    JsonBool True -> 2
    JsonNum neg num e ->
      3
        + numIf neg 1
        + numIf (isDecimal num) 2
        + numIf (isJust e) 4
        + numIf (maybe False expIsNegative e) 4
    JsonStr _ -> 15
    JsonArr _ -> 16
    JsonObj _ -> 17
    JsonRef _ -> 18
 where
  numIf cond n = if cond then n else 0

-- | Encodes a json number using the provided encoding settings.
encodejsonNumber :: EncodingSettings -> JsonNumber -> BSB.Builder
encodejsonNumber settings (JsonInt i) = encodeNumber (intSize settings) i
encodejsonNumber settings (JsonDecimal i d) =
  encodeNumber (intSize settings) i <> encodeNumber (decimalSize settings) d

-- | Encodes an exponent using the provided encoding settings.
encodeExponent :: EncodingSettings -> JsonExponent -> BSB.Builder
encodeExponent settings (JsonExponent _ e) = encodeNumber (decimalSize settings) e

-- | Encodes a single json value using the provided encoding settings and jbon object definitions.
encodeValue :: EncodingSettings -> Indexed JbonObject -> JsonValue -> BSB.Builder
encodeValue _ _ n@JsonNull = valueTypeNumber n
encodeValue _ _ b@(JsonBool _) = valueTypeNumber b
encodeValue settings _ n@(JsonNum _ num e) =
  valueTypeNumber n
    <> encodejsonNumber settings num
    <> maybe mempty (encodeExponent settings) e
encodeValue settings _ s@(JsonStr str) =
  valueTypeNumber s <> encodeStr (stringLength settings) str
encodeValue settings objs a@(JsonArr arr) =
  valueTypeNumber a <> encodeLength (arrayLength settings) arr
    <> mconcat (encodeValue settings objs <$> arr)
encodeValue settings objs o@(JsonObj fields) =
  valueTypeNumber o
    <> encodeNumber (numberOfObjects settings) (getObjectId fields objs)
    <> mconcat (encodeValue settings objs . snd <$> fields)
encodeValue settings _ r@(JsonRef idx) =
  valueTypeNumber r
    <> encodeNumber (numberOfReferences settings) idx

-- Optimization

-- | Replace all duplicate entries with references to a single instance of this value.
gatherDuplicates :: EncodingSettings -> JsonValue -> (JsonValue, Indexed JsonValue)
gatherDuplicates settings val = go 0 [] val valueCounts
 where
  valueCounts = sortOn (Down . valueSize settings . fst) . Map.toList $ countValues val

  go :: Word64 -> Indexed JsonValue -> JsonValue -> [(JsonValue, Word64)] -> (JsonValue, Indexed JsonValue)
  go idx acc value counts =
    case uncons counts of
      Nothing -> (value, acc)
      Just ((biggestVal, count), xs)
        | refSize biggestVal count < valSize biggestVal count ->
          let -- Count the number of times all values occur in the biggest value.
              -- Subtract this number  times the number of times the value occurs, minus one, since it is still defined
              -- in the references, and values can also be replaced in there.
              innerValueCounts = (* (count - 1)) <$> countValues biggestVal

              -- Reduce the counts of the values, no need to reorder, since their sizes have not changed (we only
              -- replaced values that are bigger than the ones in the list so far).
              newCounts = subtractNums xs innerValueCounts

              -- Reduce the main value.
              reducedValue = replaceValue biggestVal (JsonRef idx) value

              -- Reduce all of the referenced values.
              reducedAcc = second (replaceValue biggestVal (JsonRef idx)) <$> acc
           in go (idx + 1) ((idx, biggestVal) : reducedAcc) reducedValue newCounts
        | otherwise -> go idx acc value xs

  -- The size of count references to a value in jbon.
  refSize :: JsonValue -> Word64 -> Word64
  refSize val' count =
    refSize' -- Reference id.
      + valueSize settings val' -- Reference value definition.
      + refSize' * count -- References to reference id.
   where
    refSize' = wordSizeNoBytes $ wordSize count

  -- The size of a value in jbon, given that it is used count times, without references.
  valSize :: JsonValue -> Word64 -> Word64
  valSize val' count = valueSize settings val' * count

-- | Returns the size in bytes that a json value will take up in Jbon encoding.
valueSize :: EncodingSettings -> JsonValue -> Word64
valueSize _ JsonNull = 1
valueSize _ (JsonBool _) = 1
valueSize settings (JsonNum _ (JsonInt _) e) =
  1
    + wordSizeNoBytes (intSize settings)
    + if isJust e then wordSizeNoBytes $ exponentSize settings else 0
valueSize settings (JsonNum _ (JsonDecimal _ _) e) =
  1 + wordSizeNoBytes (intSize settings)
    + wordSizeNoBytes (decimalSize settings)
    + if isJust e then wordSizeNoBytes $ exponentSize settings else 0
valueSize settings (JsonStr str) =
  1 + wordSizeNoBytes (stringLength settings)
    + genericLength (BSL.unpack $ BSB.toLazyByteString $ BSB.stringUtf8 str)
valueSize settings (JsonArr arr) =
  1 + wordSizeNoBytes (arrayLength settings)
    + sum (valueSize settings <$> arr)
valueSize settings (JsonObj fields) =
  1 + wordSizeNoBytes (numberOfObjects settings)
    + sum (valueSize settings . snd <$> fields)
valueSize settings (JsonRef _) = 1 + wordSizeNoBytes (numberOfReferences settings)

{- | Finds all values in a JsonValue that have duplicates, and the number of times they occur.
 | The values are returned as a list sorted by their size in descending order.
-}
countValues :: JsonValue -> Map.Map JsonValue Word64
countValues = Map.filter (> 1) . go Map.empty
 where
  go :: Map.Map JsonValue Word64 -> JsonValue -> Map.Map JsonValue Word64
  go acc arr@(JsonArr elems) = foldl' go (increment arr acc) elems
  go acc obj@(JsonObj fields) = foldl' go (increment obj acc) (snd <$> fields)
  go acc leaf = increment leaf acc

  increment :: JsonValue -> Map.Map JsonValue Word64 -> Map.Map JsonValue Word64
  increment = Map.alter (Just . maybe 1 (+ 1))
