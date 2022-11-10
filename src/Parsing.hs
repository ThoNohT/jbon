module Parsing (
  Parser (..),
  liftP,
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
) where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isDigit, isSpace)
import Data.Word (Word64)
import Text.Read (readMaybe)

-- | A parser.
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)} deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  Parser pf <*> Parser pv = Parser $ pf >=> \(rest, f) -> second f <$> pv rest

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \input -> a input <|> b input

instance Monad Parser where
  Parser a >>= f = Parser $ a >=> \(rest, a') -> runParser (f a') rest

-- | Lifts a Maybe into a Parser that doesn't consume anything but returns the value if Some.
liftP :: Maybe a -> Parser a
liftP value = Parser $ \input -> (input,) <$> value

-- | A parser that parses a single character.
pChar :: Char -> Parser Char
pChar c = Parser $ \case
  x : xs | x == c -> Just (xs, x)
  _ -> Nothing

-- | A parser that parses a specific string.
pString :: String -> Parser String
pString s = sequenceA $ pChar <$> s

-- | A parser that returns the next character if it passes a check, and fails otherwise.
pCond :: (Char -> Bool) -> Parser Char
pCond f = Parser $ \case
  x : xs | f x -> Just (xs, x)
  _ -> Nothing

{- | A parser that returns whether the next character passes a check, and consumes it. If the character doesn't pass
 | the check, then nothing is consumed. If there is no input, returns False.
-}
pCheck :: (Char -> Bool) -> Parser Bool
pCheck f = Parser $ \case
  x : xs | f x -> Just (xs, True)
  xs -> Just (xs, False)

-- | A parser that parses a span that satisfies a condition.
pSpan :: (Char -> Bool) -> Parser String
pSpan = many . pCond

-- | A parser that fails another parser if it resulted in an empty list.
notNull :: Parser [a] -> Parser [a]
notNull p = p >>= liftP . \v -> if null v then Nothing else Just v

-- | A parser that parses an integer.
pInt :: Parser Word64
pInt = liftP . readMaybe =<< notNull (pSpan isDigit)

-- | A parser that parses whitespace.
ws :: Parser String
ws = pSpan isSpace

-- | A parser that parses and discards any whitespace around another parser.
inWs :: Parser a -> Parser a
inWs p = ws *> p <* ws

{- | A parser that takes a separator parser and an element parser and runs the element parser multiple times as long as
 | the separator parser succeeds in between every successive run.
-}
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep elmt = (:) <$> elmt <*> many (sep *> elmt) <|> pure []

-- | A parser that takes another parser, and fails if it leaves any input.
entire :: Parser a -> Parser a
entire (Parser p) = Parser $ p >=> \(rest, result) -> if null rest then pure (rest, result) else Nothing
