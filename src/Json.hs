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
import Data.List (genericLength, intercalate)
import Data.Word (Word64)
import Parsing (Parser (..), entire, inWs, pChar, pCheck, pCond, pInt, pString, sepBy, ws)

-- | A type the wraps all possible values that can exist inside a json document.
data JsonValue where
  JsonNull :: JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNum :: Bool -> JsonNumber -> JsonValue
  JsonStr :: String -> JsonValue
  JsonArr :: [JsonValue] -> JsonValue
  JsonObj :: [(String, JsonValue)] -> JsonValue
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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

encodeJsonNumber :: JsonNumber -> String
encodeJsonNumber (JsonInt i) = show i
encodeJsonNumber (JsonDecimal i d) = show i <> "." <> show d

encodeField :: (String, JsonValue) -> String
encodeField (name, val) = show name <> ":" <> encodeJsonValue val
