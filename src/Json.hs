module Json (
  JsonValue (..),
  JsonNumber (..),
  parseJsonValue,
  encodeJsonValue,
  maxStringLength,
  maxArrayLength,
  maxInt,
  maxDecimal,
) where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (void)
import Core (safeMaximum)
import Data.List (genericLength, intercalate, intersperse)
import Data.Word (Word64)
import Formattable (Formattable (..), format', indent, unlines')
import Parsing (Parser (..), entire, inWs, pChar, pCheck, pCond, pInt, pString, sepBy, ws)

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

instance Formattable JsonValue where
  formattedLength JsonNull = 4
  formattedLength (JsonBool True) = 4
  formattedLength (JsonBool False) = 5
  formattedLength (JsonNum negative num) = formattedLength num + if negative then 1 else 0
  formattedLength (JsonStr str) = genericLength str + 2
  formattedLength (JsonArr arr) = 4 + sum (intersperse 3 $ formattedLength <$> arr)
  formattedLength (JsonRef index) = genericLength (show index) + 6
  formattedLength (JsonObj fields) = 4 + sum (intersperse 3 $ fieldLength <$> fields)
   where
    fieldLength (name, value) = genericLength name + 2 + 2 + formattedLength value

  formatSingleLine JsonNull = "null"
  formatSingleLine (JsonBool val) = if val then "true" else "false"
  formatSingleLine (JsonNum neg num) = (if neg then "-" else "") <> formatSingleLine num
  formatSingleLine (JsonStr str) = "\"" <> str <> "\""
  formatSingleLine (JsonRef index) = "<<" <> show index <> ">>"
  formatSingleLine (JsonArr arr) = "[ " <> intercalate ", " (formatSingleLine <$> arr) <> " ]"
  formatSingleLine (JsonObj fields) =
    "{ " <> intercalate ", " ((\(n, v) -> "\"" <> n <> "\": " <> formatSingleLine v) <$> fields) <> " }"

  formatMultiline a@(JsonArr arr) = Just $ \idnt start maxLen ->
    if formattedLength a + fromIntegral start <= maxLen
      then formatSingleLine a
      else
        unlines'
          [ "["
          , unlines' $ commaSeparate [] 1 $ format' (idnt + 1) (idnt * 2) maxLen <$> arr
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
    formatPair idnt maxLen (name, value) =
      "\"" <> name <> "\": " <> format' idnt (idnt + genericLength name + 4) maxLen value
  formatMultiline _ = Nothing

commaSeparate :: [String] -> Int -> [String] -> [String]
commaSeparate _ _ [] = []
commaSeparate acc idnt [x] = reverse $ indent idnt x : acc
commaSeparate acc idnt (x : xs) = commaSeparate (indent idnt x <> "," : acc) idnt xs

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

{- | For convenience, a json number is separated into decimals and integers, based on whether there is a decimal separator.
 | The first Bool indicates whether the number is negative.
-}
data JsonNumber where
  JsonDecimal :: Word64 -> Word64 -> JsonNumber
  JsonInt :: Word64 -> JsonNumber
  deriving (Eq, Show, Ord)

instance Formattable JsonNumber where
  formattedLength (JsonDecimal i d) = genericLength (show i) + 1 + genericLength (show d)
  formattedLength (JsonInt i) = genericLength (show i)

  formatSingleLine (JsonDecimal i d) = show i <> "." <> show d
  formatSingleLine (JsonInt i) = show i

  formatMultiline _ = Nothing

-- | Attempts to parse a json value from a string.
parseJsonValue :: String -> Either String JsonValue
parseJsonValue input = fst <$> runParser (entire "End of file" $ inWs "Surrounding ws" jsonValue) input
 where
  jsonNull :: Parser String JsonValue
  jsonNull = JsonNull <$ pString "null"

  jsonBool :: Parser String JsonValue
  jsonBool = JsonBool True <$ pString "true" <|> JsonBool False <$ pString "false"

  jsonNumber :: Parser String JsonValue
  jsonNumber = JsonNum <$> negP <*> (decP <|> intP)
   where
    negP = pCheck (== '-')

    decP = do
      pre <- pInt "Int part of decimal"
      void $ pChar "Decimal separator" '.'
      JsonDecimal pre <$> pInt "Decimal part of decimal"

    intP = JsonInt <$> pInt "Int"

  stringLiteral :: Parser String String
  stringLiteral =
    concat
      <$> ( pChar "Starting quote of string" '"'
              *> many stringCharP <* pChar "Ending quote of string" '"'
          )
   where
    stringCharP :: Parser String String
    stringCharP = escaped <|> unescaped

    unescaped = (: []) <$> pCond "Unescaped char" (\c -> c /= '"' && c /= '\\')

    escaped = do
      pre <- pChar "Escape backslash" '\\'
      ch <- pCond "Escaped char" (const True)
      pure [pre, ch]

  jsonString :: Parser String JsonValue
  jsonString = JsonStr <$> stringLiteral

  jsonArray :: Parser String JsonValue
  jsonArray = JsonArr <$> (pChar "Array opening bracket" '[' <* ws "Array opening whitespace" *> elements <* (ws "Array closing whitespace" *> pChar "Array closing bracket" ']'))
   where
    elements = sepBy (inWs "Whitespace around array separator" $ pChar "Array separator" ',') jsonValue

  jsonObject :: Parser String JsonValue
  jsonObject = do
    void $ pChar "Object opening bracket" '{' <* ws "Object opening whitespace"
    values <- sepBy (inWs "Whitespace around object field separator" $ pChar "Object field separator" ',') pair
    void $ ws "Object closing whitespace" *> pChar "Object closing bracket" '}'
    pure $ JsonObj values
   where
    pair = do
      key <- stringLiteral
      void $ inWs "Whitespace around object assignment sign" $ pChar "Object assignment sign" ':'
      value <- jsonValue
      pure (key, value)

  jsonValue :: Parser String JsonValue
  jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

encodeJsonValue :: JsonValue -> String
encodeJsonValue JsonNull = "null"
encodeJsonValue (JsonBool False) = "false"
encodeJsonValue (JsonBool True) = "true"
encodeJsonValue (JsonNum isNeg num) = if isNeg then "-" else "" <> encodeJsonNumber num
encodeJsonValue (JsonStr str) = "\"" <> str <> "\""
encodeJsonValue (JsonArr values) = "[" <> intercalate ", " (encodeJsonValue <$> values) <> "]"
encodeJsonValue (JsonObj fields) = "{" <> intercalate ", " (encodeField <$> fields) <> "}"
-- This should not be json encoded, but just so it shows something in case.
encodeJsonValue (JsonRef idx) = "<<" <> show idx <> ">>"

encodeJsonNumber :: JsonNumber -> String
encodeJsonNumber (JsonInt i) = show i
encodeJsonNumber (JsonDecimal i d) = show i <> "." <> show d

encodeField :: (String, JsonValue) -> String
encodeField (name, val) = show name <> ":" <> encodeJsonValue val
