module Jbon.Decode (decodeJbonValue) where

import Control.Monad (replicateM, void)
import Core (notMinBound)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Word (Word64)
import Indexed (Indexed, index, indexed)
import Jbon.Encode (EncodingSettings (..), WordSize (..), w16ToSettings)
import Jbon.Jbon (JbonObject (..), mergeObject)
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
decodeJbonValue :: ByteString -> Maybe (EncodingSettings, Indexed JbonObject) -- Maybe (JsonValue, Indexed JbonObject)
decodeJbonValue input = fst <$> runParser jbonDocument input
 where
  -- jbonDocument :: Parser ByteString jbonDocument
  jbonDocument = do
    void jbonHeader
    settings <- liftP . w16ToSettings =<< pWord16
    defs <- definitions settings
    objects <- liftP $ buildDefinitions defs

    pure (settings, objects)

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

  -- Assumes that in the input, dependencies are only possible on objects defined further to the left,
  -- so processing an object that depends on something means the depended upon object was already processed.
  buildDefinitions :: Indexed (Maybe Word64, Indexed String) -> Maybe (Indexed JbonObject)
  buildDefinitions deps = go [] deps
   where
    go :: Indexed JbonObject -> Indexed (Maybe Word64, Indexed String) -> Maybe (Indexed JbonObject)
    go acc [] = Just $ reverse acc
    go acc ((idx, (dep, fields)) : xs) = case dep of
      Nothing -> go ((idx, JbonObject dep fields (snd <$> fields)) : acc) xs
      Just depIdx -> case index depIdx acc of
        Nothing -> Nothing -- Invalid dependency reference.
        Just parent -> go ((idx, mergeObject depIdx parent fields) : acc) xs
