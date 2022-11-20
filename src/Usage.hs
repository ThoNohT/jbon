module Usage (showUsage) where

import Data.Maybe (listToMaybe)

-- | Shows the usage message.
showUsage :: [String] -> IO ()
showUsage params = do
  putStrLn "Usage:"
  case listToMaybe params of
    Just "test" -> testUsage
    Just "decode" -> decodeUsage
    Just "encode" -> encodeUsage
    Just "analyze" -> analyzeUsage
    _ -> mainUsage

-- | Main usage message showing available commands.
mainUsage :: IO ()
mainUsage = do
  putStrLn "jbon encode [opts]: Encode a json value to jbon."
  putStrLn "jbon decode [opts]: Decode a jbon value to json."
  putStrLn "jbon analyze <mode> [opts]: Analyze a json or jbon value."
  putStrLn "jbon test [opts]: Run tests."
  putStrLn "jbon help [cmd]: Show this help text."
  putStrLn "  Provide the name of a command to show more help about this command and the optional parameters it takes."

testUsage :: IO ()
testUsage = do
  putStrLn "jbon test <mode> [test name]"
  putStrLn "Modes:"
  putStrLn "  - run: Runs all tests."
  putStrLn "  - run <test name>: Runs the specified test."
  putStrLn "  - update <test name>: Runs the specified test."

decodeUsage :: IO ()
decodeUsage = do
  putStrLn "jbon decode [options]"
  putStrLn "Attempts to decode a jbon file to json."
  putStrLn "Options:"
  putStrLn "  -i <fileName>: The filename to read from. If not specified stdin is used."
  putStrLn "  -o <fileName>: The filename to write to. If not specified stdout is used."
  putStrLn "  -f: If specified, the json is output formatted over multiple lines."
  putStrLn "  -l <length>: The number of characters per line to aim for. If not specified, 80 is used."
  putStrLn "               Only has an effect if -f is also specified."
  putStrLn "  -s: If specified, nothing is written to stdout when outputting to a file."

encodeUsage :: IO ()
encodeUsage = do
  putStrLn "jbon encode [options]"
  putStrLn "Attempts to encode a json file to jbon."
  putStrLn "Options:"
  putStrLn "  -i <fileName>: The filename to read from. If not specified stdin is used."
  putStrLn "  -o <fileName>: The filename to write to. If not specified stdout is used."
  putStrLn "  -s: If specified, nothing is written to stdout when outputting to a file."

analyzeUsage :: IO ()
analyzeUsage = do
  putStrLn "jbon analyze <mode> [options]"
  putStrLn "Modes:"
  putStrLn "  - json: Analyzes a json file."
  putStrLn "  - jbon: Analyzes a jbon file."
  putStrLn "Options:"
  putStrLn "  -i <fileName>: The filename to read from. If not specified stdin is used."
  putStrLn "  -o <fileName>: The filename to write to. If not specified stdout is used."
  putStrLn "  -s: If specified, nothing is written to stdout when outputting to a file."
