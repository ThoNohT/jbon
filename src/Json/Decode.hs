module Json.Decode (parseJsonValue) where

import Control.Applicative (Alternative ((<|>)), many, optional)
import Control.Monad (void)
import Data.Maybe (isJust)
import Json.Json (JsonExponent (..), JsonNumber (..), JsonValue (..))
import Parsing (
  Parser (..),
  entire,
  inWs,
  pChar,
  pCheck,
  pCond,
  pInt,
  pString,
  sepBy,
  ws,
 )

-- | Attempts to parse a json value from a string.
parseJsonValue :: String -> Either String JsonValue
parseJsonValue input = fst <$> runParser (entire "End of file" $ inWs "Surrounding ws" jsonValue) input
 where
  jsonNull :: Parser String JsonValue
  jsonNull = JsonNull <$ pString "null"

  jsonBool :: Parser String JsonValue
  jsonBool = JsonBool True <$ pString "true" <|> JsonBool False <$ pString "false"

  jsonNumber :: Parser String JsonValue
  jsonNumber = JsonNum <$> negP <*> (decP <|> intP) <*> optional expP
   where
    negP = pCheck (== '-')

    decP = do
      pre <- pInt "Int part of decimal"
      void $ pChar "Decimal separator" '.'
      JsonDecimal pre <$> pInt "Decimal part of decimal"

    intP = JsonInt <$> pInt "Int"

    expP =
      (\_ n e -> JsonExponent (isJust n) e)
        <$> (pChar "Exponent sign" 'E' <|> pChar "Exponent sign" 'e')
          <*> optional (pChar "Exponent negative sign" '-')
          <*> pInt "Exponent"

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
