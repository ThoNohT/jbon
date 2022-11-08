module Main (main) where

import Bson (indexed, tryGetIndexedSubList)
import Json (parseJsonValue)

-- Json parsers.
main :: IO ()
main = do
  print $ parseJsonValue "null"
  print $ parseJsonValue "true"
  print $ parseJsonValue "false"
  print $ parseJsonValue "13"
  print $ parseJsonValue "-13"
  print $ parseJsonValue "13.342"
  print $ parseJsonValue "-1234.342"
  print $ parseJsonValue "\"Some \\a string\\\"\""
  print $ parseJsonValue "\"\""
  print $ parseJsonValue "[13, 12.5, true, null, \"Hey!\", [false, true]]"
  print $ parseJsonValue "[]"
  print $ parseJsonValue "[null]"
  print $ parseJsonValue "{}"
  print $ parseJsonValue "{ \"test\": 25, \"x\": null,  \"five\"  : \"five\",  \"arr\": [1,2,3, {}]  }"

  print $ indexed @String 1 ["a", "b"]

  print $
    tryGetIndexedSubList
      (indexed 1 ["a", "b"])
      (indexed 1 ["0", "0b", "a", "1", "1b", "b", "2", "2b", "2c"])
