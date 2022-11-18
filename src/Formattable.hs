module Formattable (Formattable (..), format', format, indentation, unlines', indent) where
import Data.List (intercalate)

-- | Type class for things that can be formatted.
class Formattable a where
  -- | The length of this value, if it were formatted over a single line.
  formattedLength :: a -> Integer

  -- | Formats a value over a single line.
  formatSingleLine :: a -> String

  -- | Formats a value over multiple lines, if the type supports it, and if it is needed.
  -- | Indentation -> StartForLengthCheck -> MaxLineLength -> Output
  formatMultiline :: a -> Maybe (Int -> Int -> Integer -> String)

-- | Calls either formatSingleLine or formatMultiline on a value, depending on whether it supports multiline formatting.
format' :: forall a. Formattable a => Int -> Int -> Integer -> a -> String
format' idnt startPos maxLength value =
  case formatMultiline value of
    Just f -> f idnt startPos maxLength
    Nothing -> formatSingleLine value

-- | Helper to call formatting starting without indentation.
format :: forall a. Formattable a => Integer -> a -> String
format = format' 0 0

-- | Generates whitespace for indentation of the specied length.
indentation :: Int -> String
indentation idnt = replicate (idnt * 2) ' '

-- | Indents all lines of a string.
indent :: Int -> String -> String
indent idnt = unlines' . fmap (\x -> indentation idnt <> x) . lines

-- | Unlines without trailing newline.
unlines' :: [String] -> String
unlines' = intercalate "\n"
