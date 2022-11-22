module Json.Json (
  JsonValue (..),
  JsonNumber (..),
  JsonExponent (..),
  isDecimal,
  expIsNegative,
  replaceValue,
  expandJsonValue,
  maxStringLength,
  maxArrayLength,
  maxInt,
  maxDecimal,
  maxExponent,
) where

import Core (safeMaximum)
import Data.Bifunctor (Bifunctor (second))
import Data.List (genericLength, genericReplicate, intercalate, intersperse)
import Data.Word (Word64)
import Formattable (Formattable (..), format', indent, unlines')
import Indexed (Indexed, index)

{- | For convenience, a json number is separated into decimals and integers, based on whether there is a decimal separator.
 | The first Bool indicates whether the number is negative.
-}
data JsonNumber where
  JsonDecimal :: Word64 -> Word64 -> JsonNumber
  JsonInt :: Word64 -> JsonNumber
  deriving (Eq, Show, Ord)

-- | Indicates whether a JsonNumber is a decimal.
isDecimal :: JsonNumber -> Bool
isDecimal (JsonInt _) = False
isDecimal (JsonDecimal _ _) = True

-- | A json exponent can be positive or negative, and has a number.
data JsonExponent where
  JsonExponent :: Bool -> Word64 -> JsonExponent
  deriving (Eq, Show, Ord)

-- | Indicates whether a JsonExponent is negative.
expIsNegative :: JsonExponent -> Bool
expIsNegative (JsonExponent neg _) = neg

-- | A type the wraps all possible values that can exist inside a json document.
data JsonValue where
  JsonNull :: JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNum :: Bool -> JsonNumber -> Maybe JsonExponent -> JsonValue
  JsonStr :: String -> JsonValue
  JsonArr :: [JsonValue] -> JsonValue
  JsonCountedArr :: [(Word64, JsonValue)] -> JsonValue
  JsonObj :: [(String, JsonValue)] -> JsonValue
  -- This value cannot be parsed, only created by making references for duplicate values.
  JsonRef :: Word64 -> JsonValue
  deriving (Eq, Show, Ord)

-- | Determines the length of the longest string in a json value.
maxStringLength :: JsonValue -> Word64
maxStringLength = \case
  JsonStr str -> genericLength str
  JsonArr arr -> safeMaximum $ maxStringLength <$> arr
  JsonCountedArr arr -> safeMaximum $ maxStringLength . snd <$> arr
  JsonObj objs -> safeMaximum $ maxStringLength . snd <$> objs
  _ -> 0

-- | Determines the length of the longest array in a json value.
maxArrayLength :: JsonValue -> Word64
maxArrayLength = \case
  JsonArr arr -> max (genericLength arr) (safeMaximum $ maxArrayLength <$> arr)
  JsonCountedArr arr -> max (genericLength arr) (safeMaximum $ maxArrayLength . snd <$> arr)
  JsonObj objs -> safeMaximum $ maxArrayLength . snd <$> objs
  _ -> 0

-- | Determines the maximum integer value in a json value.
maxInt :: JsonValue -> Word64
maxInt = \case
  JsonNum _ (JsonInt i) _ -> i
  JsonNum _ (JsonDecimal i _) _ -> i
  JsonArr arr -> safeMaximum $ maxInt <$> arr
  JsonCountedArr arr -> safeMaximum $ maxInt . snd <$> arr
  JsonObj objs -> safeMaximum $ maxInt . snd <$> objs
  _ -> 0

-- | Determines the maximum decimal value in a json value.
maxDecimal :: JsonValue -> Word64
maxDecimal = \case
  JsonNum _ (JsonDecimal _ d) _ -> d
  JsonArr arr -> safeMaximum $ maxInt <$> arr
  JsonObj objs -> safeMaximum $ maxInt . snd <$> objs
  _ -> 0

-- | Determines the maximum exponent in a json value.
maxExponent :: JsonValue -> Word64
maxExponent = \case
  JsonNum _ _ (Just (JsonExponent _ e)) -> e
  JsonArr arr -> safeMaximum $ maxExponent <$> arr
  JsonCountedArr arr -> safeMaximum $ maxExponent . snd <$> arr
  JsonObj objs -> safeMaximum $ maxExponent . snd <$> objs
  _ -> 0

-- | Replace every instance of the first value with the second value in the third value.
replaceValue :: JsonValue -> JsonValue -> JsonValue -> JsonValue
replaceValue toReplace replaceWith replaceIn =
  case replaceIn of
    _ | replaceIn == toReplace -> replaceWith
    JsonArr arr -> JsonArr $ replaceValue toReplace replaceWith <$> arr
    JsonCountedArr arr -> JsonCountedArr $ second (replaceValue toReplace replaceWith) <$> arr
    JsonObj fields -> JsonObj $ second (replaceValue toReplace replaceWith) <$> fields
    v -> v

{- | Expands a json value by replacing referenes from the values in the provided mapping.
 | Returns Nothing if any reference could not be found.
-}
expandJsonValue :: Indexed JsonValue -> JsonValue -> Maybe JsonValue
expandJsonValue refs (JsonRef idx) = index idx refs >>= (\v -> if mustExpand v then expandJsonValue refs v else Just v)
expandJsonValue refs (JsonArr arr) = JsonArr <$> mapM (expandJsonValue refs) arr
expandJsonValue refs (JsonCountedArr arr) = JsonArr . concat <$> mapM expandSingle arr
 where
  expandSingle (c, v) = genericReplicate c <$> expandJsonValue refs v
expandJsonValue refs (JsonObj fields) = JsonObj <$> mapM (\(n, v) -> (n,) <$> expandJsonValue refs v) fields
expandJsonValue _ v = Just v

-- | Indicates whether a json value has references or a counted array, and therefore needs expanding.
mustExpand :: JsonValue -> Bool
mustExpand (JsonRef _) = True
mustExpand (JsonArr arr) = any mustExpand arr
mustExpand (JsonCountedArr _) = True
mustExpand (JsonObj fields) = any mustExpand (snd <$> fields)
mustExpand _ = False

-- Formatting

instance Formattable JsonNumber where
  formattedLength (JsonDecimal i d) = genericLength (show i) + 1 + genericLength (show d)
  formattedLength (JsonInt i) = genericLength (show i)

  formatSingleLine (JsonDecimal i d) = show i <> "." <> show d
  formatSingleLine (JsonInt i) = show i

  formatMultiline _ = Nothing

instance Formattable JsonExponent where
  formattedLength (JsonExponent neg e) = if neg then 2 else 1 + genericLength (show e)

  formatSingleLine (JsonExponent neg e) = "E" <> (if neg then "-" else "") <> show e

  formatMultiline _ = Nothing

instance Formattable JsonValue where
  formattedLength JsonNull = 4
  formattedLength (JsonBool True) = 4
  formattedLength (JsonBool False) = 5
  formattedLength (JsonNum negative num e) =
    if negative then 1 else 0 + formattedLength num + maybe 0 formattedLength e
  formattedLength (JsonStr str) = genericLength str + 2
  formattedLength (JsonArr arr) = 4 + sum (intersperse 3 $ formattedLength <$> arr)
  formattedLength (JsonCountedArr arr) = 4 + sum (intersperse 3 $ (\(c, v) -> 3 + genericLength (show c) + formattedLength v) <$> arr)
  formattedLength (JsonRef idx) = genericLength (show idx) + 6
  formattedLength (JsonObj fields) = 4 + sum (intersperse 3 $ fieldLength <$> fields)
   where
    fieldLength (name, value) = genericLength name + 2 + 2 + formattedLength value

  formatSingleLine JsonNull = "null"
  formatSingleLine (JsonBool val) = if val then "true" else "false"
  formatSingleLine (JsonNum neg num e) =
    (if neg then "-" else "") <> formatSingleLine num <> maybe "" formatSingleLine e
  formatSingleLine (JsonStr str) = "\"" <> str <> "\""
  formatSingleLine (JsonRef idx) = "<<" <> show idx <> ">>"
  formatSingleLine (JsonArr arr) = "[ " <> intercalate ", " (formatSingleLine <$> arr) <> " ]"
  formatSingleLine (JsonCountedArr arr) =
    "[ " <> intercalate ", " ((\(c, v) -> "(" <> show c <> "," <> formatSingleLine v <> ")") <$> arr) <> " ]"
  formatSingleLine (JsonObj fields) =
    "{ " <> intercalate ", " ((\(n, v) -> "\"" <> n <> "\": " <> formatSingleLine v) <$> fields) <> " }"

  formatMultiline a@(JsonArr arr) = Just $ \idnt start maxLen ->
    if formattedLength a + fromIntegral start <= maxLen
      then formatSingleLine a
      else
        unlines'
          [ "["
          , (unlines' $ commaSeparate [] 1 (format' (idnt + 1) (idnt * 2) maxLen <$> arr)) :: String
          , "]"
          ]
  formatMultiline a@(JsonCountedArr arr) = Just $ \idnt start maxLen ->
    if formattedLength a + fromIntegral start <= maxLen
      then formatSingleLine a
      else
        unlines'
          [ "["
          , (unlines' $ commaSeparate [] 1 ((\(c, v) -> "(" <> show c <> "," <> format' (idnt + 1) (idnt * 2) maxLen v <> ")") <$> arr)) :: String
          , "]"
          ]
  formatMultiline o@(JsonObj fields) = Just $ \idnt start maxLen ->
    if formattedLength o + fromIntegral start <= maxLen
      then formatSingleLine o
      else
        unlines'
          [ "{"
          , unlines' $ commaSeparate [] 1 $ formatPair (idnt + 1) maxLen <$> fields
          , "}"
          ]
   where
    formatPair :: Int -> Integer -> (String, JsonValue) -> String
    formatPair idnt maxLen (name, value) =
      "\"" <> name <> "\": " <> format' idnt (idnt + genericLength name + 4) maxLen value
  formatMultiline _ = Nothing

commaSeparate :: [String] -> Int -> [String] -> [String]
commaSeparate _ _ [] = []
commaSeparate acc idnt [x] = reverse $ indent idnt x : acc
commaSeparate acc idnt (x : xs) = commaSeparate (indent idnt x <> "," : acc) idnt xs
