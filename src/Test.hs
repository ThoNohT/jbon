module Test (run, runSingleTest, update) where

import Core (indexed)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Internal as BSI (w2c)
import Data.ByteString.Lazy as BSL (ByteString, readFile, unpack, writeFile)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (find)
import Jbon.Build (getObjectDefinitions, minify, tryGetIndexedSubList)
import Jbon.Encode (encode)
import Json (JsonNumber (..), JsonValue (..), parseJsonValue)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)

-- | A test, with a name and the result of running it.
data Test = Test {testName :: String, runTest :: ByteString}

-- | Helpre to build jbon objects and encode them in one go for tests.
buildAndDecode :: JsonValue -> Builder
buildAndDecode value = encode objs value where objs = getObjectDefinitions value

-- | A list containing all available tests.
tests :: [Test]
tests =
  [ Test
      "parsing json"
      ( pack $
          unlines
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
  , Test "indexed" (pack $ show $ indexed @String 1 ["a", "b"])
  , Test
      "tryGetIndexedSubList"
      ( pack $
          show $
            tryGetIndexedSubList
              (indexed 1 ["a", "b"])
              (indexed 1 ["0", "0b", "a", "1", "1b", "b", "2", "2b", "2c"])
      )
  , Test "minify" (pack $ show $ minify [["a", "b"], ["a"], ["a", "b", "c"], ["b", "c"], ["b", "c"], ["x"]])
  , Test
      "getObjectDefinitions"
      ( pack $
          unlines
            [ show $ getObjectDefinitions $ JsonObj [("a", JsonNull), ("b", JsonNull)]
            , show $ getObjectDefinitions $ JsonObj []
            , show $
                getObjectDefinitions $
                  JsonArr
                    [ JsonObj [("a", JsonNull)]
                    , JsonObj [("a", JsonNull)]
                    , JsonObj [("a", JsonNull), ("b", JsonNull)]
                    , JsonObj
                        [ ("a", JsonNull)
                        ,
                          ( "b"
                          , JsonObj
                              [ ("a", JsonNull)
                              , ("b", JsonNull)
                              , ("c", JsonArr [JsonObj [], JsonObj [("d", JsonNull)]])
                              ]
                          )
                        ]
                    ]
            ]
      )
  , Test "encode-empty" (toLazyByteString $ buildAndDecode (JsonObj []))
  , Test "encode-single" (toLazyByteString $ buildAndDecode (JsonObj [("a", JsonBool True)]))
  , Test
      "encode-nested"
      ( toLazyByteString $
          buildAndDecode (JsonArr [JsonObj [("a", JsonNull)], JsonObj [("a", JsonNum False (JsonInt 23))]])
      )
  , Test
      "encode-advanced"
      ( toLazyByteString $
          buildAndDecode
            ( JsonObj
                [ ("a", JsonStr "Hello!\\\"")
                , ("b", JsonBool True)
                , ("c", JsonObj [("b", JsonNull)])
                ]
            )
      )
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
runSingleTest' :: String -> ByteString -> IO ()
runSingleTest' name result = do
  let fp = "./Tests/" <> name <> ".txt"
  exists <- doesFileExist fp
  if not exists
    then statusMsg "ERROR" "No results file found."
    else do
      fileContents <- BSL.readFile fp
      if fileContents == result
        then statusMsg "OK" "Test succeeded."
        else do
          statusMsg "ERROR" "Test result mismatch, got:"
          putStrLn $ BSI.w2c <$> BSL.unpack result
          putStrLn "Expected:"
          putStrLn $ BSI.w2c <$> BSL.unpack fileContents
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
      putStrLn $ BSI.w2c <$> BSL.unpack result
      BSL.writeFile fp result
      putStrLn $ "Written to " <> fp <> "."
