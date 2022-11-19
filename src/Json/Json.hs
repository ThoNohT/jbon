module Json.Json (
  JsonValue (..),
  JsonNumber (..),
  replaceValue,
  expandJsonValue,
  maxStringLength,
  maxArrayLength,
  maxInt,
  maxDecimal,
) where

import Core (safeMaximum)
import Data.Bifunctor (Bifunctor (second))
import Data.List (genericLength, intercalate, intersperse)
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

-- | A type the wraps all possible values that can exist inside a json document.
data JsonValue where
  JsonNull :: JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNum :: Bool -> JsonNumber -> JsonValue
  JsonStr :: String -> JsonValue
  JsonArr :: [JsonValue] -> JsonValue
  JsonObj :: [(String, JsonValue)] -> JsonValue
  -- This value cannot be parsed, only created by making references for duplicate values.
  JsonRef :: Word64 -> JsonValue
  deriving (Eq, Show, Ord)

-- | Determines the length of the longest string in a json value.
maxStringLength :: JsonValue -> Word64
maxStringLength = \case
  JsonStr str -> genericLength str
  JsonArr arr -> safeMaximum $ maxStringLength <$> arr
  JsonObj objs -> safeMaximum $ maxStringLength . snd <$> objs
  _ -> 0

-- | Determines the length of the longest array in a json value.
maxArrayLength :: JsonValue -> Word64
maxArrayLength = \case
  JsonArr arr -> max (genericLength arr) (safeMaximum $ maxArrayLength <$> arr)
  JsonObj objs -> safeMaximum $ maxArrayLength . snd <$> objs
  _ -> 0

-- | Determines the maximum integer value in a json value.
maxInt :: JsonValue -> Word64
maxInt = \case
  JsonNum _ (JsonInt i) -> i
  JsonNum _ (JsonDecimal i _) -> i
  JsonArr arr -> (safeMaximum $ maxInt <$> arr)
  JsonObj objs -> safeMaximum $ maxInt . snd <$> objs
  _ -> 0

-- | Determines the maximum decimal value in a json value.
maxDecimal :: JsonValue -> Word64
maxDecimal = \case
  JsonNum _ (JsonDecimal _ d) -> d
  JsonArr arr -> (safeMaximum $ maxInt <$> arr)
  JsonObj objs -> safeMaximum $ maxInt . snd <$> objs
  _ -> 0

-- | Replace every instance of the first value with the second value in the third value.
replaceValue :: JsonValue -> JsonValue -> JsonValue -> JsonValue
replaceValue toReplace replaceWith replaceIn =
  case replaceIn of
    _ | replaceIn == toReplace -> replaceWith
    JsonArr arr -> JsonArr $ replaceValue toReplace replaceWith <$> arr
    JsonObj fields -> JsonObj $ second (replaceValue toReplace replaceWith) <$> fields
    v -> v

{- | Expands a json value by replacing referenes from the values in the provided mapping.
 | Returns Nothing if any reference could not be found.
-}
expandJsonValue :: Indexed JsonValue -> JsonValue -> Maybe JsonValue
expandJsonValue refs (JsonRef idx) = index idx refs >>= (\v -> if hasRefs v then expandJsonValue refs v else Just v)
expandJsonValue refs (JsonArr arr) = JsonArr <$> mapM (expandJsonValue refs) arr
expandJsonValue refs (JsonObj fields) = JsonObj <$> mapM (\(n, v) -> (n,) <$> expandJsonValue refs v) fields
expandJsonValue _ v = Just v

-- | Indicates whether a json value has references.
hasRefs :: JsonValue -> Bool
hasRefs (JsonRef _) = True
hasRefs (JsonArr arr) = any hasRefs arr
hasRefs (JsonObj fields) = any hasRefs (snd <$> fields)
hasRefs _ = False

-- Formatting

instance Formattable JsonNumber where
  formattedLength (JsonDecimal i d) = genericLength (show i) + 1 + genericLength (show d)
  formattedLength (JsonInt i) = genericLength (show i)

  formatSingleLine (JsonDecimal i d) = show i <> "." <> show d
  formatSingleLine (JsonInt i) = show i

  formatMultiline _ = Nothing

instance Formattable JsonValue where
  formattedLength JsonNull = 4
  formattedLength (JsonBool True) = 4
  formattedLength (JsonBool False) = 5
  formattedLength (JsonNum negative num) = formattedLength num + if negative then 1 else 0
  formattedLength (JsonStr str) = genericLength str + 2
  formattedLength (JsonArr arr) = 4 + sum (intersperse 3 $ formattedLength <$> arr)
  formattedLength (JsonRef idx) = genericLength (show idx) + 6
  formattedLength (JsonObj fields) = 4 + sum (intersperse 3 $ fieldLength <$> fields)
   where
    fieldLength (name, value) = genericLength name + 2 + 2 + formattedLength value

  formatSingleLine JsonNull = "null"
  formatSingleLine (JsonBool val) = if val then "true" else "false"
  formatSingleLine (JsonNum neg num) = (if neg then "-" else "") <> formatSingleLine num
  formatSingleLine (JsonStr str) = "\"" <> str <> "\""
  formatSingleLine (JsonRef idx) = "<<" <> show idx <> ">>"
  formatSingleLine (JsonArr arr) = "[ " <> intercalate ", " (formatSingleLine <$> arr) <> " ]"
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
