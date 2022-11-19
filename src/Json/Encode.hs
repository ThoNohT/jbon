module Json.Encode (encodeJsonValue) where

import Data.List (intercalate)
import Json.Json (JsonNumber (..), JsonValue (..))

encodeJsonNumber :: JsonNumber -> String
encodeJsonNumber (JsonInt i) = show i
encodeJsonNumber (JsonDecimal i d) = show i <> "." <> show d

encodeJsonValue :: JsonValue -> String
encodeJsonValue JsonNull = "null"
encodeJsonValue (JsonBool False) = "false"
encodeJsonValue (JsonBool True) = "true"
encodeJsonValue (JsonNum isNeg num) = if isNeg then "-" else "" <> encodeJsonNumber num
encodeJsonValue (JsonStr str) = "\"" <> str <> "\""
-- This should not be json encoded, but just so it shows something in case.
encodeJsonValue (JsonRef idx) = "<<" <> show idx <> ">>"
encodeJsonValue (JsonArr values) = "[" <> intercalate ", " (encodeJsonValue <$> values) <> "]"
encodeJsonValue (JsonObj fields) = "{" <> intercalate ", " (encodeField <$> fields) <> "}"
 where
  encodeField :: (String, JsonValue) -> String
  encodeField (name, val) = show name <> ":" <> encodeJsonValue val
