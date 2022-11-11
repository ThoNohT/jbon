module Json (JsonValue (..), JsonNumber (..), parseJsonValue, maxStringLength, maxArrayLength, maxInt, maxDecimal) where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (void)
import Core (safeMaximum)
import Data.List (genericLength)
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
parseJsonValue :: String -> Maybe JsonValue
parseJsonValue input = snd <$> runParser (entire $ inWs jsonValue) input
 where
  jsonNull :: Parser JsonValue
  jsonNull = JsonNull <$ pString "null"

  jsonBool :: Parser JsonValue
  jsonBool = (JsonBool True <$ pString "true") <|> (JsonBool False <$ pString "false")

  jsonNumber :: Parser JsonValue
  jsonNumber = JsonNum <$> negP <*> (decP <|> intP)
   where
    negP = pCheck (== '-')

    decP = do
      pre <- pInt
      void $ pChar '.'
      post <- pInt
      pure $ JsonDecimal pre post

    intP = JsonInt <$> pInt

  stringLiteral :: Parser String
  stringLiteral = concat <$> (pChar '"' *> many stringCharP <* pChar '"')
   where
    stringCharP :: Parser String
    stringCharP = escaped <|> unescaped

    unescaped = (: []) <$> pCond (\c -> c /= '"' && c /= '\\')

    escaped = do
      pre <- pChar '\\'
      ch <- pCond (const True)
      pure [pre, ch]

  jsonString :: Parser JsonValue
  jsonString = JsonStr <$> stringLiteral

  jsonArray :: Parser JsonValue
  jsonArray = JsonArr <$> ((pChar '[' <* ws) *> elements <* inWs (ws *> pChar ']'))
   where
    elements = sepBy (inWs $ pChar ',') jsonValue

  jsonObject :: Parser JsonValue
  jsonObject = do
    void $ pChar '{' <* ws
    values <- sepBy (inWs $ pChar ',') pair
    void $ ws *> pChar '}'
    pure $ JsonObj values
   where
    pair = do
      key <- stringLiteral
      void $ inWs $ pChar ':'
      value <- jsonValue
      pure (key, value)

  jsonValue :: Parser JsonValue
  jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
