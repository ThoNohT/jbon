module Parsing (
  Uncons (..),
  Parser (..),
  liftP,
  pElem,
  pChar,
  pString,
  pCond,
  pCheck,
  pSpan,
  notNull,
  pInt,
  ws,
  inWs,
  sepBy,
  entire,
  pWord8,
  pWord16,
  pWord32,
  pWord64,
) where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (shift)
import Data.ByteString qualified as BS (ByteString, uncons)
import Data.ByteString.Char8 qualified as BSC (uncons)
import Data.ByteString.Internal as BSI (w2c)
import Data.ByteString.Lazy qualified as BSL (ByteString, uncons)
import Data.Char (isDigit, isSpace)
import Data.List qualified as L (uncons)
import Data.Word (Word16, Word32, Word64, Word8)
import Text.Read (readMaybe)

{- | Type class for sequence types that can be unconsed, taking the first element from the sequence,
 | and returning this element and the remaining sequence, or None if the sequence is empty.
-}
class Uncons l a where
  uncons :: l -> Maybe (a, l)

-- | All lists implement it with uncons from the List module.
instance Uncons [a] a where uncons = L.uncons

-- | Uncons for Strict ByteString.
instance Uncons BS.ByteString Word8 where uncons = BS.uncons

-- | Uncons for Strict Bytestring with Chars.
instance Uncons BS.ByteString Char where uncons = BSC.uncons

-- | Uncons for Lazy ByteString.
instance Uncons BSL.ByteString Word8 where uncons = BSL.uncons

-- | Uncons for Lazy ByteString with Chars.
instance Uncons BSL.ByteString Char where uncons l = first BSI.w2c <$> BSL.uncons l

-- | A parser.
newtype Parser l a = Parser {runParser :: l -> Maybe (a, l)} deriving (Functor)

instance Applicative (Parser l) where
  pure a = Parser $ \input -> Just (a, input)
  Parser pf <*> Parser pv = Parser $ pf >=> \(f, rest) -> first f <$> pv rest

instance Alternative (Parser l) where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \input -> a input <|> b input

instance Monad (Parser l) where
  Parser a >>= f = Parser $ a >=> \(a', rest) -> runParser (f a') rest

-- Generic parsers.

-- | Lifts a Maybe into a Parser that doesn't consume anything but returns the value if Some.
liftP :: forall l a. Maybe a -> Parser l a
liftP value = Parser $ \input -> (,input) <$> value

-- | A parser that returns the next character if it passes a check, and fails otherwise.
pCond :: forall l a. Uncons l a => (a -> Bool) -> Parser l a
pCond f = Parser $ \input -> case uncons input of
  Just (x, xs) | f x -> Just (x, xs)
  _ -> Nothing

-- | A parser that parses a span that satisfies a condition.
pSpan :: forall l a. Uncons l a => (a -> Bool) -> Parser l [a]
pSpan = many . pCond

{- | A parser that returns whether the next character passes a check, and consumes it. If the character doesn't pass
 | the check, then nothing is consumed. If there is no input, returns False.
-}
pCheck :: forall l a. Uncons l a => (a -> Bool) -> Parser l Bool
pCheck f = Parser $ \input -> case uncons input of
  Just (x, xs) | f x -> Just (True, xs)
  _ -> Just (False, input)

-- | A parser that fails another parser if it resulted in an empty list.
notNull :: forall l a. Parser l [a] -> Parser l [a]
notNull p = p >>= liftP . \v -> if null v then Nothing else Just v

{- | A parser that takes a separator parser and an element parser and runs the element parser multiple times as long as
 | the separator parser succeeds in between every successive run.
-}
sepBy :: forall l a b. Parser l a -> Parser l b -> Parser l [b]
sepBy sep elmt = (:) <$> elmt <*> many (sep *> elmt) <|> pure []

-- TODO: Figure out how to make this work.
-- end :: forall l a. Uncons l a => Parser l ()
-- end = Parser $ \input -> case uncons @l @a input of
--   Just _ -> Nothing
--   Nothing -> Just ((), input)

endC :: forall l. Uncons l Char => Parser l ()
endC = Parser $ \input -> case uncons input of
  Just (_ :: Char, _) -> Nothing
  Nothing -> Just ((), input)

-- | A parser that takes another parser, and fails if it leaves any input.
entire :: forall l a. Uncons l Char => Parser l a -> Parser l a
entire p = p <* endC

-- Char based parsers.

-- | A parser that parses one element of the specified type.
pElem :: forall l a. Uncons l a => Parser l a
pElem = pCond (const True)

-- | A parser that parses a single character.
pChar :: forall l. Uncons l Char => Char -> Parser l Char
pChar c = pCond (== c)

-- | A parser that parses a specific string.
pString :: forall l. Uncons l Char => String -> Parser l String
pString s = sequenceA $ pChar <$> s

-- | A parser that parses an integer.
pInt :: forall l. Uncons l Char => Parser l Word64
pInt = liftP . readMaybe =<< notNull (pSpan isDigit)

-- | A parser that parses whitespace.
ws :: forall l. Uncons l Char => Parser l String
ws = pSpan isSpace

-- | A parser that parses and discards any whitespace around another parser.
inWs :: forall l a. Uncons l Char => Parser l a -> Parser l a
inWs p = ws *> p <* ws

-- Word based parsers.

-- | A parser that parses a Word8.
pWord8 :: forall l. Uncons l Word8 => Parser l Word8
pWord8 = Parser uncons

-- | A parser that parses a Word16.
pWord16 :: forall l. Uncons l Word8 => Parser l Word16
pWord16 = do
  b1 :: Word16 <- fromIntegral <$> pWord8
  b0 :: Word16 <- fromIntegral <$> pWord8
  pure $ shift b0 8 + b1

-- | A parser that parses a Word32.
pWord32 :: forall l. Uncons l Word8 => Parser l Word32
pWord32 = do
  b1 :: Word32 <- fromIntegral <$> pWord16
  b0 :: Word32 <- fromIntegral <$> pWord16
  pure $ shift b0 16 + b1

-- | A parser that parses a Word64.
pWord64 :: forall l. Uncons l Word8 => Parser l Word64
pWord64 = do
  b1 :: Word64 <- fromIntegral <$> pWord32
  b0 :: Word64 <- fromIntegral <$> pWord32
  pure $ shift b0 16 + b1
