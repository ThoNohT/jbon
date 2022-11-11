module Jbon.Jbon (JbonObject (..), getObjectId, nFields, maxFieldLength) where

import Core (Indexed, safeMaximum)
import Data.List (find, genericLength)
import Data.Maybe (fromJust)
import Data.Word (Word64)

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
