module Jbon.Decode (decodeJbonValue) where

import Control.Applicative (Alternative (empty))
import Control.Monad (replicateM, void)
import Core (notMinBound)
import Data.ByteString (pack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.UTF8 as BSU (toString)
import Data.Word (Word64, Word8)
import Indexed (Indexed, index, indexed)
import Jbon.Build (buildDefinitions)
import Jbon.Encode (EncodingSettings (..), WordSize (..), w16ToSettings)
import Jbon.Jbon (JbonObject (..))
import Json (JsonNumber (..), JsonValue (..))
import Parsing (Parser (..), liftP, pElem, pString, pWord16, pWord32, pWord64, pWord8, withError)

-- | Parses a number with the provided size.
parseNumber :: String -> WordSize -> Parser ByteString Word64
parseNumber err W8 = fromIntegral <$> pWord8 err
parseNumber err W16 = fromIntegral <$> pWord16 err
parseNumber err W32 = fromIntegral <$> pWord32 err
parseNumber err W64 = pWord64 err

-- | Parses a string, where the length of the string is encoded with the provided size.
parseString :: String -> WordSize -> Parser ByteString String
parseString err size = do
  len <- parseNumber (err <> " string length") size
  w8s <- withError "String" $ replicateM (fromIntegral len) $ pElem (err <> " string")
  pure $ BSU.toString $ pack w8s

-- | Decodes a byte string into a json value and its jbon definition. Returns Nothing if decoding fails.
decodeJbonValue :: ByteString -> Either String (EncodingSettings, JsonValue, Indexed JbonObject)
decodeJbonValue input = fst <$> runParser jbonDocument input
 where
  jbonDocument :: Parser ByteString (EncodingSettings, JsonValue, Indexed JbonObject)
  jbonDocument = do
    void jbonHeader
    settings <- liftP "Settings conversion" . w16ToSettings =<< pWord16 "Jbon settings w16"
    defs <- definitions settings
    objects <- liftP "Definition building" $ buildDefinitions defs
    value <- jsonValue settings objects

    pure (settings, value, objects)

  jbonHeader :: Parser ByteString String
  jbonHeader = pString "JBON"

  definitions :: EncodingSettings -> Parser ByteString (Indexed (Maybe Word64, Indexed String))
  definitions settings = do
    numberOfObjects <- parseNumber "Number of objects" (numberOfObjects settings)
    objects <- replicateM (fromIntegral numberOfObjects) (definition settings)
    pure $ indexed 1 objects

  definition :: EncodingSettings -> Parser ByteString (Maybe Word64, Indexed String)
  definition settings = do
    inherit <- parseNumber "Object inherit" (numberOfObjects settings)
    numberOfFields <- parseNumber "Nuber of fields" (numberOfFields settings)
    fields <- replicateM (fromIntegral numberOfFields) (field settings)
    pure (notMinBound inherit, fields)

  field :: EncodingSettings -> Parser ByteString (Word64, String)
  field settings = do
    idx <- parseNumber "Number of fields" (numberOfFields settings)
    name <- parseString "Field name" (stringLength settings)
    pure (idx, name)

  jsonValue :: EncodingSettings -> Indexed JbonObject -> Parser ByteString JsonValue
  jsonValue settings objects = pWord8 "Value type id" >>= go
   where
    go :: Word8 -> Parser ByteString JsonValue
    go 0 = pure JsonNull
    go 1 = pure $ JsonBool False
    go 2 = pure $ JsonBool True
    go 3 = JsonNum False . JsonInt <$> parseNumber "Pos Json int int" (intSize settings)
    go 4 = JsonNum True . JsonInt <$> parseNumber "Neg Json int int" (intSize settings)
    go 5 = JsonNum False <$> decimal
    go 6 = JsonNum True <$> decimal
    go 7 = JsonStr <$> parseString "Json string" (stringLength settings)
    go 8 = do
      arrLen <- parseNumber "Array length" (arrayLength settings)
      JsonArr <$> withError "Array" (replicateM (fromIntegral arrLen) (jsonValue settings objects))
    go 9 = do
      objIndex <- parseNumber "Object index" (numberOfObjects settings)
      JbonObject _ _ fields <- liftP "Finding object by index" $ index objIndex objects
      JsonObj <$> mapM (\n -> (n,) <$> jsonValue settings objects) fields
    go i = withError ("Unknown element " <> show i) empty

    decimal :: Parser ByteString JsonNumber
    decimal =
      JsonDecimal <$> parseNumber "Json decimal int" (intSize settings)
        <*> parseNumber "Json decimal decimal" (decimalSize settings)
