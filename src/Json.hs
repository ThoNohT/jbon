module Json (JsonValue (..), JsonNumber (..), parseJsonValue) where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (void)
import Parsing (Parser (..), entire, inWs, pChar, pCond, pCheck, pInt, pString, sepBy, ws)

-- | A type the wraps all possible values that can exist inside a json document.
data JsonValue where
  JsonObj :: [(String, JsonValue)] -> JsonValue
  JsonArr :: [JsonValue] -> JsonValue
  JsonStr :: String -> JsonValue
  JsonNum :: Bool -> JsonNumber -> JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNull :: JsonValue
  deriving (Eq, Show)

-- | For convenience, a json number is separated into decimals and integers, based on whether there is a decimal separator.
-- | The first Bool indicates whether the number is negative.
data JsonNumber where
  JsonDecimal :: Integer -> Integer -> JsonNumber
  JsonInt :: Integer -> JsonNumber
  deriving (Eq, Show)

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
