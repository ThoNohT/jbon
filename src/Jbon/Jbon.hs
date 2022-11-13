module Jbon.Jbon (JbonObject (..), getObjectId, nFields, maxFieldLength, mergeObject) where

import Core (safeMaximum)
import Data.List (find, genericLength)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Indexed (Indexed, indexed)

{- | Definition of a jbon object:
 - From which object index it inherits.
 - Which fields are present this object, besides the ones inherited.
 - All fields present in this object.
-}
data JbonObject = JbonObject (Maybe Word64) (Indexed String) [String] deriving (Eq, Show)

-- | Returns the number of fields in this object.
nFields :: JbonObject -> Word64
nFields (JbonObject _ _ fields) = genericLength fields

-- | Determines the length of the longest field name.
maxFieldLength :: JbonObject -> Word64
maxFieldLength (JbonObject _ _ fields) = safeMaximum $ genericLength <$> fields

-- | Determines the identifier of the jbon object that has the provided fields.
getObjectId :: forall a. [(String, a)] -> [(Word64, JbonObject)] -> Word64
getObjectId fields objs =
  fromJust $ fst <$> find (\(_, JbonObject _ _ objFields) -> (fst <$> fields) == objFields) objs

{- | Merges an object and a list of field definitions into a new object, inserting the elements in the
 specified locations.
-}
mergeObject :: Word64 -> JbonObject -> Indexed String -> JbonObject
mergeObject parentIdx (JbonObject _ _ fields) newFields =
  JbonObject (Just parentIdx) newFields $ go [] (indexed 1 fields) newFields
 where
  go :: [String] -> Indexed String -> Indexed String -> [String]
  go acc [] [] = reverse acc
  go acc fs [] = reverse acc <> (snd <$> fs)
  go acc [] es = reverse acc <> (snd <$> es)
  go acc ((fi, f) : fs) ((ei, e) : es) =
    if ei < fi
      then go (e : acc) ((fi, f) : fs) es
      else go (f : acc) fs ((ei, e) : es)
