module Jbon.Decode (decodeJbonValue) where

import Control.Applicative (Alternative (empty))
import Control.Monad (replicateM, void)
import Core (notMinBound)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Word (Word64, Word8)
import Indexed (Indexed, index, indexed)
import Jbon.Build (buildDefinitions)
import Jbon.Encode (EncodingSettings (..), WordSize (..), w16ToSettings)
import Jbon.Jbon (JbonObject (..))
import Json (JsonNumber (..), JsonValue (..))
import Parsing (Parser (..), liftP, pElem, pString, pWord16, pWord32, pWord64, pWord8)

-- | Parses a number with the provided size.
parseNumber :: WordSize -> Parser ByteString Word64
parseNumber W8 = fromIntegral <$> pWord8
parseNumber W16 = fromIntegral <$> pWord16
parseNumber W32 = fromIntegral <$> pWord32
parseNumber W64 = pWord64

-- | Parses a string, where the length of the string is encoded with the provided size.
parseString :: WordSize -> Parser ByteString String
parseString size = do
  len <- parseNumber size
  replicateM (fromIntegral len) pElem

-- | Decodes a byte string into a json value and its jbon definition. Returns Nothing if decoding fails.
decodeJbonValue :: ByteString -> Maybe (EncodingSettings, JsonValue, Indexed JbonObject)
decodeJbonValue input = fst <$> runParser jbonDocument input
 where
  jbonDocument :: Parser ByteString (EncodingSettings, JsonValue, Indexed JbonObject)
  jbonDocument = do
    void jbonHeader
    settings <- liftP . w16ToSettings =<< pWord16
    defs <- definitions settings
    objects <- liftP $ buildDefinitions defs
    value <- jsonValue settings objects

    pure (settings, value, objects)

  jbonHeader :: Parser ByteString String
  jbonHeader = pString "JBON"

  definitions :: EncodingSettings -> Parser ByteString (Indexed (Maybe Word64, Indexed String))
  definitions settings = do
    numberOfObjects <- parseNumber (numberOfObjects settings)
    objects <- replicateM (fromIntegral numberOfObjects) (definition settings)
    pure $ indexed 1 objects

  definition :: EncodingSettings -> Parser ByteString (Maybe Word64, Indexed String)
  definition settings = do
    inherit <- parseNumber (numberOfObjects settings)
    numberOfFields <- parseNumber (numberOfFields settings)
    fields <- replicateM (fromIntegral numberOfFields) (field settings)
    pure (notMinBound inherit, fields)

  field :: EncodingSettings -> Parser ByteString (Word64, String)
  field settings = do
    idx <- parseNumber (numberOfFields settings)
    value <- parseString (stringLength settings)
    pure (idx, value)

  jsonValue :: EncodingSettings -> Indexed JbonObject -> Parser ByteString JsonValue
  jsonValue settings objects = pWord8 >>= go
   where
    go :: Word8 -> Parser ByteString JsonValue
    go 0 = pure JsonNull
    go 1 = pure $ JsonBool False
    go 2 = pure $ JsonBool True
    go 3 = JsonNum False . JsonInt <$> parseNumber (intSize settings)
    go 4 = JsonNum True . JsonInt <$> parseNumber (intSize settings)
    go 5 = JsonNum False <$> decimal
    go 6 = JsonNum True <$> decimal
    go 7 = JsonStr <$> parseString (stringLength settings)
    go 8 = do
      arrLen <- parseNumber (arrayLength settings)
      JsonArr <$> replicateM (fromIntegral arrLen) (jsonValue settings objects)
    go 9 = do
      objIndex <- parseNumber (numberOfObjects settings)
      JbonObject _ _ fields <- liftP $ index objIndex objects
      JsonObj <$> mapM (\n -> (n,) <$> jsonValue settings objects) fields
    go _ = empty

    decimal :: Parser ByteString JsonNumber
    decimal = JsonDecimal <$> parseNumber (intSize settings) <*> parseNumber (decimalSize settings)
