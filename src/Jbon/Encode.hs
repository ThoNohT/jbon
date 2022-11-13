module Jbon.Encode (EncodingSettings (..), WordSize (..), encode, settingsToW16, w16ToSettings) where

import Core (safeMaximum)
import Data.Bits (Bits ((.&.)), shift)
import Data.ByteString.Builder qualified as BSB
import Data.List (genericLength)
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word32, Word64, Word8)
import Jbon.Jbon (JbonObject (..), getObjectId, maxFieldLength, nFields)
import Json (JsonNumber (..), JsonValue (..), maxArrayLength, maxDecimal, maxInt, maxStringLength)

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
wordSizeBits :: WordSize -> Word16
wordSizeBits W8 = 0
wordSizeBits W16 = 1
wordSizeBits W32 = 2
wordSizeBits W64 = 3

-- | TODO: Documentation.
bitsWordSize :: Word16 -> Maybe WordSize
bitsWordSize 0 = Just W8
bitsWordSize 1 = Just W16
bitsWordSize 2 = Just W32
bitsWordSize 3 = Just W64
bitsWordSize _ = Nothing

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
  }
  deriving (Show)

settingsToW16 :: EncodingSettings -> Word16
settingsToW16 settings =
  wordSizeBits (numberOfObjects settings)
    + shift (wordSizeBits $ numberOfFields settings) 2
    + shift (wordSizeBits $ arrayLength settings) 4
    + shift (wordSizeBits $ stringLength settings) 6
    + shift (wordSizeBits $ objectNameLength settings) 8
    + shift (wordSizeBits $ intSize settings) 10
    + shift (wordSizeBits $ decimalSize settings) 12

w16ToSettings :: Word16 -> Maybe EncodingSettings
w16ToSettings word =
  EncodingSettings <$> bitsWordSize (word .&. 3)
    <*> bitsWordSize (shift word (-2) .&. 3)
    <*> bitsWordSize (shift word (-4) .&. 3)
    <*> bitsWordSize (shift word (-6) .&. 3)
    <*> bitsWordSize (shift word (-8) .&. 3)
    <*> bitsWordSize (shift word (-10) .&. 3)
    <*> bitsWordSize (shift word (-12) .&. 3)

-- | Creates encoding settings given a json value, and the calculated jbon objects.
makeSettings :: JsonValue -> [(Word64, JbonObject)] -> EncodingSettings
makeSettings value objs =
  EncodingSettings
    { numberOfObjects = wordSize $ genericLength objs
    , numberOfFields = wordSize $ safeMaximum $ nFields . snd <$> objs
    , arrayLength = wordSize $ maxArrayLength value
    , stringLength = wordSize $ maxStringLength value
    , objectNameLength = wordSize $ safeMaximum $ maxFieldLength . snd <$> objs
    , intSize = wordSize $ maxInt value
    , decimalSize = wordSize $ maxDecimal value
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
encodeStr stringLength str = encodeLength stringLength str <> BSB.stringUtf8 str

-- Jbon related encoders

-- | Encodes a json value to jbon.
encode :: [(Word64, JbonObject)] -> JsonValue -> BSB.Builder
encode objs value = BSB.string8 "JBON" <> settingsHeader <> objsHeader <> body
 where
  settings = makeSettings value objs

  settingsHeader = BSB.word16LE $ settingsToW16 settings

  objsHeader :: BSB.Builder
  objsHeader =
    encodeLength (numberOfObjects settings) objs
      <> mconcat (encodeJbonObject settings . snd <$> objs)

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

-- | Encodes a single json value using the provided encoding settings and jbon object definitions.
encodeValue :: EncodingSettings -> [(Word64, JbonObject)] -> JsonValue -> BSB.Builder
encodeValue _ _ JsonNull = BSB.word8 0
encodeValue _ _ (JsonBool False) = BSB.word8 1
encodeValue _ _ (JsonBool True) = BSB.word8 2
encodeValue settings _ (JsonNum False (JsonInt i)) = BSB.word8 3 <> encodeNumber (intSize settings) i
encodeValue settings _ (JsonNum True (JsonInt i)) = BSB.word8 4 <> encodeNumber (intSize settings) i
encodeValue settings _ (JsonNum False (JsonDecimal i d)) =
  BSB.word8 5 <> encodeNumber (intSize settings) i
    <> encodeNumber (decimalSize settings) d
encodeValue settings _ (JsonNum True (JsonDecimal i d)) =
  BSB.word8 6 <> encodeNumber (intSize settings) i
    <> encodeNumber (decimalSize settings) d
encodeValue settings _ (JsonStr str) = BSB.word8 7 <> encodeStr (stringLength settings) str
encodeValue settings objs (JsonArr arr) =
  BSB.word8 8 <> encodeLength (arrayLength settings) arr
    <> mconcat (encodeValue settings objs <$> arr)
encodeValue settings objs (JsonObj fields) =
  BSB.word8 9
    <> encodeNumber (numberOfObjects settings) (getObjectId fields objs)
    <> mconcat (encodeValue settings objs . snd <$> fields)
