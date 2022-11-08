module Main (main) where

import Json (parseJsonValue)

-- Json parsers.
main :: IO ()
main = do
  print $ parseJsonValue "null"
  print $ parseJsonValue "true"
  print $ parseJsonValue "false"
  print $ parseJsonValue "13"
  print $ parseJsonValue "13.342"
  print $ parseJsonValue "\"Some \\a string\\\"\""
  print $ parseJsonValue "\"\""
  print $ parseJsonValue "[13, 12.5, true, null, \"Hey!\", [false, true]]"
  print $ parseJsonValue "[]"
  print $ parseJsonValue "[null]"
  print $ parseJsonValue "{}"
  print $ parseJsonValue "{ test: 25, x: null,  five  : \"five\",  arr: [1,2,3, {}]  }"
