module Main (main) where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL (getContents, putStr, readFile, writeFile)
import Data.Char (toLower)
import Data.List (elemIndex, isPrefixOf, uncons)
import Data.Maybe (fromMaybe, listToMaybe)
import Formattable (format)
import Jbon.Build (getObjectDefinitions)
import Jbon.Decode (decodeJbonValue)
import Jbon.Encode (applyReferences, encodeJbon, makeSettings)
import Json.Decode (parseJsonValue)
import Json.Encode (encodeJsonValue)
import Json.Json (JsonValue)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test qualified (run, runSingleTest, update)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Usage qualified

main :: IO ()
main = do
  args <- getArgs
  let args' = uncons args
  case args' of
    Just (cmd, params)
      | cmd == "encode" -> encode params
      | cmd == "decode" -> decode params
      | cmd == "test" -> test params
      | cmd == "analze" -> analyze params
      | cmd == "help" -> Usage.showUsage params
      | otherwise -> do
        Usage.showUsage []
        putStrLn $ "Invalid command: '" <> cmd <> "'."
    Nothing -> do
      Usage.showUsage []
      putStrLn "No command provided."

-- | Checks whether a parameter is defined in the provided list of parameters.
boolParam :: String -> [String] -> Bool
boolParam name = elem ("-" <> name)

{- | Checks whethere a parameter is defined, and if so, returns the next value after it.
 | If there is no value after it, an empty string is returned.
 | If the next parameter starts with a '-'  character, it is assumed to not belong to this parameter.
-}
strParam :: String -> [String] -> Maybe String
strParam name params =
  case elemIndex ("-" <> name) params of
    Nothing -> Nothing
    Just idx ->
      let (_, post) = splitAt (idx + 1) params
          arg = fromMaybe "" $ listToMaybe post
       in Just $ if "-" `isPrefixOf` arg then "" else arg

{- | Attempts to get a file parameter, if the value of the parameter is empty,
 | then an error is shown and the application will exit.
-}
getFileParam :: String -> String -> [String] -> IO (Maybe FilePath)
getFileParam usageMode name params = do
  case strParam name params of
    Nothing -> pure Nothing
    Just "" -> do
      Usage.showUsage [usageMode]
      putStrLn $ "Empty file parameter '" <> name <> "' provided."
      exitFailure
    Just p -> pure $ Just p

{- | Attempts to get a parameter of a type that implements the Read typeclass.
 | If the value is defined, but cannot be read, the provided error message will be shown and the application will
 | exit.
-}
getReadParam :: forall a. Read a => String -> String -> String -> [String] -> IO (Maybe a)
getReadParam usageMode errorMsg name params = do
  case strParam name params of
    Nothing -> pure Nothing
    Just str ->
      case readMaybe @a str of
        Nothing -> do
          Usage.showUsage [usageMode]
          putStrLn errorMsg
          exitFailure
        Just v -> pure $ Just v

-- | Gets the Right part of an Either, or exits with the provided message and the error in the Left part.
getRight :: forall a. String -> String -> Either String a -> IO a
getRight _ _ (Right v) = pure v
getRight usageMode errMsg (Left err) = do
  Usage.showUsage [usageMode]
  putStrLn errMsg
  putStrLn err
  exitFailure

-- | Formats a json value given the settings provided in the parameters.
formatJsonValue :: JsonValue -> [String] -> IO String
formatJsonValue json params = do
  let formatted = boolParam "f" params
  maxLen <- fromMaybe 80 <$> getReadParam "decode" "Unable to parse the line length to a number" "l" params
  pure $ if formatted then format maxLen json else encodeJsonValue json

{- | Outputs the provided string either to standard output or to the file if it is specified.
 | If a file is specified, it is written to stdout that the value was written to this file.
-}
outputStr :: String -> [String] -> IO ()
outputStr value params = do
  outFile <- getFileParam "encode" "o" params
  case outFile of
    Nothing -> putStr value
    Just fp -> do
      writeFile fp value
      putStrLn $ printf "Written to '%s'" fp

encode :: [String] -> IO ()
encode params = do
  inFile <- getFileParam "encode" "i" params
  input <- maybe getContents readFile inFile

  json <- getRight "encode" "Unable to parse Json." $ parseJsonValue input
  let defs = getObjectDefinitions json
  let jbon = encodeJbon defs json

  outFile <- getFileParam "encode" "o" params
  case outFile of
    Nothing -> BSL.putStr $ toLazyByteString jbon
    Just fp -> do
      BSL.writeFile fp $ toLazyByteString jbon
      putStrLn $ printf "Written to '%s'" fp

decode :: [String] -> IO ()
decode params = do
  inFile <- getFileParam "decode" "i" params
  input <- maybe BSL.getContents BSL.readFile inFile

  (_, json, _, _) <- getRight "decode" "Unable to parse Jbon." $ decodeJbonValue input
  outStr <- formatJsonValue json params

  outputStr outStr params

test :: [String] -> IO ()
test params = do
  case params of
    ["run"] -> Test.run
    "run" : rest -> Test.runSingleTest $ unwords rest
    "update" : rest -> Test.update $ unwords rest
    [] -> do
      Usage.showUsage ["test"]
      putStrLn "No subcommand provided."
      exitFailure
    other -> do
      putStrLn $ "Invalid command: '" <> unwords other <> "'."

analyze :: [String] -> IO ()
analyze params = do
  case fmap toLower <$> strParam "m" params of
    Nothing -> do
      Usage.showUsage ["analyze"]
      putStrLn "No mode provided."
      exitFailure
    Just "json" -> do
      inFile <- getFileParam "encode" "i" params
      input <- maybe getContents readFile inFile
      json' <- getRight "encode" "Unable to parse Json." $ parseJsonValue input

      let defs = getObjectDefinitions json'
      let settings' = makeSettings json' defs
      let (settings, refs, json) = applyReferences settings' json'
      outVal <- formatJsonValue json params

      let outStr =
            unwords
              [ "[Definitions]:"
              , show defs
              , "\n==========\n"
              , "[Settings]:"
              , show settings
              , "\n==========\n"
              , "[References]:"
              , show refs
              , "\n==========\n"
              , "[Value]:"
              , outVal
              , "\n==========\n"
              ]

      outputStr outStr params
    Just "jbon" -> do
      inFile <- getFileParam "encode" "i" params
      input <- maybe BSL.getContents BSL.readFile inFile

      (settings, json, refs, defs) <- getRight "decode" "Unable to parse Jbon." $ decodeJbonValue input
      outVal <- formatJsonValue json params

      let outStr =
            unwords
              [ "[Definitions]:"
              , show defs
              , "\n==========\n"
              , "[Settings]:"
              , show settings
              , "\n==========\n"
              , "[References]:"
              , show refs
              , "\n==========\n"
              , "[Value]:"
              , outVal
              , "\n==========\n"
              ]

      outputStr outStr params
    Just other -> do
      Usage.showUsage ["analyze"]
      putStrLn $ "Invalid mode: '" <> other <> "'."
      exitFailure
