module Main (main) where

import Data.Map (Map)

-- | A type the wraps all possible values that can exist inside a json document.
data JsonValue where
  JsonObj :: Map String JsonValue -> JsonValue
  JsonArr :: [JsonValue] -> JsonValue
  JsonStr :: String -> JsonValue
  JsonNum :: JsonNumber -> JsonValue
  JsonBool :: Bool -> JsonValue
  JsonNull :: JsonValue
  deriving (Eq, Show)

-- | For convenience, a json number is separated into decimals and integers, based on whether there is a decimal separator.
data JsonNumber where
  JsonDecimal :: Integer -> Integer -> JsonNumber
  JsonInt :: Integer -> JsonNumber
  deriving (Eq, Show)

main :: IO ()
main = putStrLn "Hello, Bson!"
