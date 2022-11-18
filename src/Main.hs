module Main (main) where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL (readFile, writeFile)
import Formattable (format)
import Jbon.Build (getObjectDefinitions)
import Jbon.Decode (decodeJbonValue)
import Jbon.Encode (applyReferences, encodeJbon, makeSettings)
import Json (encodeJsonValue, parseJsonValue)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test qualified
import Text.Printf (printf)

-- Json parsers.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pipe"] -> pipeInput
    ["test"] -> Test.run
    ["encode", fileName] -> encodeFile fileName
    ["decode", fileName] -> decodeFile False fileName
    ["decodef", fileName] -> decodeFile True fileName
    ["analyze", "json", fileName] -> analyzeJson fileName
    ["analyze", "jbon", fileName] -> analyzeJbon fileName
    "test" : xs -> Test.runSingleTest $ unwords xs
    "update" : xs -> Test.update $ unwords xs
    "help" : _ -> showUsage
    _ -> do
      putStrLn "Invalid command."
      showUsage

pipeInput :: IO ()
pipeInput = do
  inp <- getContents

  putStr $ "Contents:\n" <> inp

encodeFile :: FilePath -> IO ()
encodeFile filePath = do
  str <- readFile filePath
  case parseJsonValue str of
    Left err -> do
      putStrLn "Unable to parse Json"
      putStrLn err
      exitFailure
    Right json -> do
      let defs = getObjectDefinitions json
      let jbon = encodeJbon defs json
      let outFn = filePath <> ".jbon"
      BSL.writeFile outFn (toLazyByteString jbon)
      putStrLn $ printf "Written to '%s'" outFn

analyzeJson :: FilePath -> IO ()
analyzeJson filePath = do
  str <- readFile filePath
  case parseJsonValue str of
    Left err -> do
      putStrLn "Unable to parse Json"
      putStrLn err
      exitFailure
    Right value' -> do
      let defs = getObjectDefinitions value'
      let settings' = makeSettings value' defs
      let (settings, refs, value) = applyReferences settings' value'

      putStrLn "[Definitions]:"
      print defs
      putStrLn "\n==========\n"
      putStrLn "[Settings]:"
      print settings
      putStrLn "\n==========\n"
      putStrLn "[References]:"
      print refs
      putStrLn "\n==========\n"
      putStrLn "[Value]:"
      print value
      putStrLn "\n==========\n"

decodeFile :: Bool -> FilePath -> IO ()
decodeFile formatVal filePath = do
  input <- BSL.readFile filePath
  case decodeJbonValue input of
    Left err -> do
      putStrLn "Unable to parse Jbon"
      putStrLn err
      exitFailure
    Right (_, json, _, _) -> do
      let outFn = filePath <> ".json"
      writeFile outFn (if formatVal then format 80 json else encodeJsonValue json)
      putStrLn $ printf "Written to '%s'" outFn

analyzeJbon :: FilePath -> IO ()
analyzeJbon filePath = do
  input <- BSL.readFile filePath
  case decodeJbonValue input of
    Left err -> do
      putStrLn "Unable to parse Jbon"
      putStrLn err
      exitFailure
    Right (settings, _, _, defs) -> do
      putStrLn "[Definitions]:"
      print defs
      putStrLn "\n==========\n"
      putStrLn "[Settings]:"
      print settings
      putStrLn "\n==========\n"

-- | Shows the usage message.
showUsage :: IO ()
showUsage = do
  putStrLn "Usage:"
  putStrLn "jbon encode <filename>: Encode the provided json file to jbon."
  putStrLn "jbon decode <filename>: Decode the provided jbon file to json."
  putStrLn "jbon analyze json <filename>: Analyze the provided json file."
  putStrLn "jbon analyze jbon <filename>: Analyze the provided jbon file."
  putStrLn "jbon test: Run all tests."
  putStrLn "jbon test <test name>: Run the test with the provided name."
  putStrLn "jbon update <test name>: Update the output for he test with the provided name."
  putStrLn "jbon help: Show this help text."
