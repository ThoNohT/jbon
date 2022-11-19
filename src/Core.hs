module Core (Formattable (..), firstJust, safeMaximum, notMinBound, subtractNums, wordsToString, stringToWords) where

import Data.ByteString.Internal qualified as BSI
import Data.Map.Strict qualified as Map (Map, lookup)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Word (Word8)

-- | Type class for things that can be formatted.
class Formattable a where
  -- | The length of this value, if it were formatted over a single line.
  formattedLength :: a -> Integer

  -- | Formats a value, with the specified indentation, and optionally over multiple lines.
  format :: a -> Integer -> Bool -> String

-- | Returns the first element in the list for which the function returns Just.
firstJust :: forall a b. (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- | A maximum that works with bounded numbers such that on an empty foldable, the minimum bound can be used.
safeMaximum :: forall t a. (Foldable t, Ord a, Bounded a) => t a -> a
safeMaximum f = if null f then minBound else maximum f

-- | Wraps a Bounded type in Maybe, where it is Nothing if it is equal to minBound.
notMinBound :: forall a. (Eq a, Bounded a) => a -> Maybe a
notMinBound x | x == minBound = Nothing
notMinBound other = Just other

{- | Subtract the values in the second map from the values in the first map. If a value becomes minBound,
 |  remove it from the map.
-}
subtractNums :: forall k n. (Ord k, Ord n, Num n, Bounded n) => [(k, n)] -> Map.Map k n -> [(k, n)]
subtractNums m1 m2 = mapMaybe subtractElem m1
 where
  subtractElem :: (k, n) -> Maybe (k, n)
  subtractElem (k, num1) =
    case num1 - fromMaybe minBound (Map.lookup k m2) of
      diff
        | diff > minBound -> Just (k, diff)
        | otherwise -> Nothing

-- | Converts an array of words directly to a string, without any encoding.
wordsToString :: [Word8] -> String
wordsToString input = BSI.w2c <$> input

-- | Converts a string directly into an array of words, without any encoding.
stringToWords:: String -> [Word8]
stringToWords input = BSI.c2w <$> input
