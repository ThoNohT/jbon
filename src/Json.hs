module Json (JsonValue (..), JsonNumber (..), parseJsonValue) where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Parsing (Parser (..), entire, inWs, notNull, pChar, pCheck, pInt, pSpan, pString, sepBy, ws)

-- | A type the wraps all possible values that can exist inside a json document.
data JsonValue where
  JsonObj :: [(String, JsonValue)] -> JsonValue
  JsonArr :: [JsonValue] -> JsonValue
  JsonStr :: String -> JsonValue
  JsonNum :: JsonNumber -> JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNull :: JsonValue
  deriving (Eq, Show)

-- | For convenience, a json number is separated into decimals and integers, based on whether there is a decimal separator.
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
  jsonNumber = JsonNum <$> (decP <|> intP)
   where
    decP = do
      pre <- pInt
      void $ pChar '.'
      post <- pInt
      pure $ JsonDecimal pre post

    intP = JsonInt <$> pInt

  jsonString :: Parser JsonValue
  jsonString = JsonStr . concat <$> (pChar '"' *> many stringCharP <* pChar '"')
   where
    stringCharP :: Parser String
    stringCharP = escaped <|> unescaped

    unescaped = (: []) <$> pCheck (\c -> c /= '"' && c /= '\\')

    escaped = do
      pre <- pChar '\\'
      ch <- pCheck (const True)
      pure [pre, ch]

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
      key <- notNull $ pSpan isAlphaNum
      void $ inWs $ pChar ':'
      value <- jsonValue
      pure (key, value)

  jsonValue :: Parser JsonValue
  jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
