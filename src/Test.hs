module Test (run, runSingleTest, update) where

import Core (wordsToString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy as BSL (ByteString, readFile, unpack, writeFile)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (find)
import Formattable (format)
import Indexed (indexed)
import Jbon.Build (getObjectDefinitions, minify, tryGetIndexedSubList)
import Jbon.Decode (decodeJbonValue)
import Jbon.Encode (EncodingSettings (..), WordSize (..), countValues, encodeJbon, gatherDuplicates, settingsToWords, wordsToSettings)
import Json.Decode (parseJsonValue)
import Json.Json (JsonExponent (..), JsonNumber (..), JsonValue (..), replaceValue)
import Parsing (pWord16, pWord8, runParser)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)

-- | A test, with a name and the result of running it.
data Test = Test {testName :: String, runTest :: ByteString}

-- | Helper to build jbon objects and encode them in one go for tests.
buildAndEncode :: JsonValue -> Builder
buildAndEncode value = encodeJbon objs value where objs = getObjectDefinitions value

-- | A list containing all available tests.
tests :: [Test]
tests =
  let testObjects :: [(String, JsonValue)] =
        [ ("empty", JsonObj [])
        , ("single", JsonObj [("a", JsonBool True)])
        , ("inherited", JsonArr [JsonObj [("a", JsonBool True)], JsonObj [("a", JsonNull), ("b", JsonNull)]])
        , ("nested", JsonArr [JsonObj [("a", JsonNull)], JsonObj [("a", JsonNum False (JsonInt 23) Nothing)]])
        ,
          ( "advanced"
          , JsonObj
              [ ("a", JsonStr "Hello!\\\"")
              , ("b", JsonBool True)
              , ("c", JsonObj [("b", JsonNull)])
              ]
          )
        , ("longarray", JsonArr $ replicate 1000 JsonNull)
        , ("arrayswlongstrings", JsonArr $ replicate 5 $ JsonArr $ replicate 5 $ JsonStr "Semi long string too")
        , ("longstring", JsonStr $ replicate 66000 'A')
        , ("utfstring", JsonArr [JsonStr "▶X", JsonBool True])
        , ("unicodeescape", JsonArr [JsonStr "\\u0010"])
        , ("int", JsonNum False (JsonInt 5) Nothing)
        , ("decimal", JsonNum False (JsonDecimal 5 6) Nothing)
        , ("negative", JsonNum True (JsonInt 5) Nothing)
        , ("exponent", JsonNum False (JsonInt 5) (Just (JsonExponent False 3)))
        , ("exponentneg", JsonNum False (JsonInt 5) (Just (JsonExponent True 3)))
        , ("decexponentneg", JsonNum False (JsonDecimal 2 1) (Just (JsonExponent True 3)))
        , ("negdecexponentneg", JsonNum True (JsonDecimal 2 1) (Just (JsonExponent True 3)))
        ,
          ( "withrefs"
          , let o =
                  JsonObj
                    [ ("name", JsonStr "A string with some size")
                    , ("name2", JsonStr "A string with some size")
                    , ("name3", JsonStr "A string with some size")
                    , ("value", JsonNum False (JsonDecimal 15 15) Nothing)
                    ]
             in JsonArr $ replicate 5 o
          )
        ]
   in [ Test
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
                , show $ parseJsonValue "1E3"
                , show $ parseJsonValue "1E-3"
                , show $ parseJsonValue "1e+3"
                , show $ parseJsonValue "-1e3"
                , show $ parseJsonValue "-1e-3"
                , show $ parseJsonValue "-1E+3"
                , show $ parseJsonValue "-1.23E+3"
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
      , Test
          "settings"
          ( let settings = EncodingSettings W8 W16 W32 W64 W64 W32 W16 W8 W32
                settingsP = (,) <$> pWord16 "Settings msb" <*> pWord8 "Settings lsb"
                dec = pack . show . wordsToSettings . fst . fromRight ((0, 0), mempty) . runParser settingsP
                enc = toLazyByteString . (\(msb, lsb) -> BSB.word16LE msb <> BSB.word8 lsb) . settingsToWords
             in dec $ enc settings
          )
      , Test
          "replaceValue"
          ( pack . unlines $
              show
                <$> [ replaceValue (JsonBool False) JsonNull $
                        JsonArr [JsonBool True, JsonBool False, JsonBool False, JsonBool False, JsonBool True]
                    , replaceValue (JsonArr [JsonNull, JsonStr "AA"]) (JsonRef 1) $
                        JsonObj
                          [ ("a", JsonArr [JsonNull, JsonStr "AA"])
                          , ("b", JsonArr [JsonNull, JsonStr "AA"])
                          , ("b", JsonArr [JsonNull, JsonStr "AAA"])
                          ]
                    ]
          )
      , Test
          "gatherDuplicates"
          ( let settings = EncodingSettings W8 W16 W32 W64 W64 W32 W16 W8 W32
             in (pack . unlines)
                  [ show $ gatherDuplicates settings $ JsonArr $ replicate 5 (JsonStr "Hello")
                  , show $ gatherDuplicates settings $ JsonArr $ replicate 5 JsonNull
                  , let arr = JsonArr $ replicate 5 (JsonBool True)
                     in show $ gatherDuplicates settings $ JsonArr [arr, arr, arr, JsonBool True]
                  , let str = JsonStr "A semi long string"
                        arr = JsonArr $ replicate 5 str
                     in show $ gatherDuplicates settings $ JsonArr [arr, arr, arr, JsonBool True, str]
                  ]
          )
      , Test "countValues" (pack $ unlines $ (\(n, v) -> n <> ": " <> show (countValues v)) <$> testObjects)
      , Test
          "decode jbon"
          $ pack . unlines $
            (\(n, v) -> n <> ": " <> show (decodeJbonValue $ toLazyByteString $ buildAndEncode v)) <$> testObjects
      , Test "format json" $
          pack . unlines $
            (\(n, v) -> n <> ": " <> format 80 v) <$> testObjects
      ]
        <> (testObjects <&> (\(n, o) -> Test ("encode-" <> n) (toLazyByteString $ buildAndEncode o)))

-- | Runs all tests.
run :: IO ()
run = do
  createDirectoryIfMissing True "./Tests/"
  mapM_ (\(Test name result) -> runSingleTest' False name result) tests

-- | Runs the test with the provided name, if it exists.
runSingleTest :: String -> IO ()
runSingleTest name = do
  let maybeTest = find ((== name) . testName) tests
  case maybeTest of
    Nothing -> putStrLn $ "Test '" <> name <> "' not found."
    Just test -> do
      createDirectoryIfMissing True "./Tests/"
      runSingleTest' True name (runTest test)

-- | Helper for running a single test.
runSingleTest' :: Bool -> String -> ByteString -> IO ()
runSingleTest' showOutputOnSuccess name result = do
  let fp = "./Tests/" <> name <> ".txt"
  let unexpectedFp = fp <> ".unexpected"
  exists <- doesFileExist fp
  if not exists
    then statusMsg "ERROR" "No results file found."
    else do
      fileContents <- BSL.readFile fp
      if fileContents == result
        then do
          if showOutputOnSuccess then putStrLn $ wordsToString $ BSL.unpack result else pure ()
          removeFileIfExists unexpectedFp
          statusMsg "OK" "Test succeeded."
        else do
          statusMsg "ERROR" "Test result mismatch, got:"
          putStrLn $ wordsToString $ BSL.unpack result
          putStrLn "Expected:"
          putStrLn $ wordsToString $ BSL.unpack fileContents
          writeOrOverwriteFile unexpectedFp result
 where
  statusMsg status msg = putStrLn $ "[" <> status <> "] '" <> name <> "': " <> msg

-- | Removes the specified file if it exists.
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = do
  exists <- doesFileExist fp
  if exists then removeFile fp else pure ()

-- | Writes the provided byte string to the provided file. If the file already exists, it is overwritten.
writeOrOverwriteFile :: FilePath -> ByteString -> IO ()
writeOrOverwriteFile fp contents = do
  removeFileIfExists fp
  BSL.writeFile fp contents
  putStrLn $ "Written to " <> fp <> "."

-- | Updates the expected result for the test with the provided name.
update :: String -> IO ()
update name = do
  let maybeTest = find ((== name) . testName) tests
  case maybeTest of
    Nothing -> putStrLn $ "Test '" <> name <> "' not found."
    Just test -> do
      let fp = "./Tests/" <> name <> ".txt"
      let unexpectedFp = fp <> ".unexpected"
      createDirectoryIfMissing True "./Tests/"

      putStrLn $ "Test '" <> name <> "' result:"
      let result = runTest test
      putStrLn $ wordsToString $ BSL.unpack result
      removeFileIfExists unexpectedFp
      writeOrOverwriteFile fp result
