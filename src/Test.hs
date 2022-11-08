module Test (run, runSingleTest, update) where

import Bson (minify, tryGetIndexedSubList)
import Core (indexed)
import Data.List (find, intercalate)
import Json (parseJsonValue)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)

-- | A test, with a name and the result of running it.
data Test = Test {testName :: String, runTest :: String}

-- | A list containing all available tests.
tests :: [Test]
tests =
  [ Test
      "parsing json"
      ( intercalate
          "\n"
          [ show $ parseJsonValue "null"
          , show $ parseJsonValue "true"
          , show $ parseJsonValue "false"
          , show $ parseJsonValue "13"
          , show $ parseJsonValue "-13"
          , show $ parseJsonValue "13.342"
          , show $ parseJsonValue "-1234.342"
          , show $ parseJsonValue "\"Some \\a string\\\"\""
          , show $ parseJsonValue "\"\""
          , show $ parseJsonValue "[13, 12.5, true, null, \"Hey!\", [false, true]]"
          , show $ parseJsonValue "[]"
          , show $ parseJsonValue "[null]"
          , show $ parseJsonValue "{}"
          , show $ parseJsonValue "{ \"test\": 25, \"x\": null,  \"five\"  : \"five\",  \"arr\": [1,2,3, {}]  }"
          ]
      )
  , Test "indexed" (show $ indexed @String 1 ["a", "b"])
  , Test
      "tryGetIndexedSubList"
      ( show $
          tryGetIndexedSubList
            (indexed 1 ["a", "b"])
            (indexed 1 ["0", "0b", "a", "1", "1b", "b", "2", "2b", "2c"])
      )
  , Test "minify" (show $ minify [["a", "b"], ["a"], ["a", "b", "c"], ["b", "c"], ["b", "c"], ["x"]])
  ]

-- | Runs all tests.
run :: IO ()
run = do
  createDirectoryIfMissing True "./Tests/"
  mapM_
    ( \(Test name result) -> do
        runSingleTest' name result
    )
    tests

-- | Runs the test with the provided name, if it exists.
runSingleTest :: String -> IO ()
runSingleTest name = do
  let maybeTest = find ((== name) . testName) tests
  case maybeTest of
    Nothing -> putStrLn $ "Test '" <> name <> "' not found."
    Just test -> do
      createDirectoryIfMissing True "./Tests/"
      runSingleTest' name (runTest test)

-- | Helper for running a single test.
runSingleTest' :: String -> String -> IO ()
runSingleTest' name result = do
  let fp = "./Tests/" <> name <> ".txt"
  exists <- doesFileExist fp
  if not exists
    then statusMsg "ERROR" "No results file found."
    else do
      fileContents <- readFile fp
      if fileContents == result
        then statusMsg "OK" "Test succeeded."
        else do
          statusMsg "ERROR" "Test result mismatch, got:"
          putStrLn result
          putStrLn "Expected:"
          putStrLn fileContents
 where
  statusMsg status msg = putStrLn $ "[" <> status <> "] '" <> name <> "': " <> msg

-- | Updates the expected result for the test with the provided name.
update :: String -> IO ()
update name = do
  let maybeTest = find ((== name) . testName) tests
  case maybeTest of
    Nothing -> putStrLn $ "Test '" <> name <> "' not found."
    Just test -> do
      let fp = "./Tests/" <> name <> ".txt"
      createDirectoryIfMissing True "./Tests/"

      exists <- doesFileExist fp
      if exists then removeFile fp else pure ()

      putStrLn $ "Test '" <> name <> "' result:"
      let result = runTest test
      putStrLn result
      writeFile fp result
      putStrLn $ "Written to " <> fp <> "."
