module Usage (showUsage) where

import Data.Maybe (listToMaybe)

-- | Shows the usage message.
showUsage :: [String] -> IO ()
showUsage params =
  case listToMaybe params of
    Just "test" -> testUsage
    Just "decode" -> decodeUsage
    Just "encode" -> encodeUsage
    Just "analyze" -> analyzeUsage
    _ -> mainUsage

-- | Main usage message showing available commands.
mainUsage :: IO ()
mainUsage = do
  putStrLn "Usage:"
  putStrLn "jbon encode <opts>: Encode a json value to jbon."
  putStrLn "jbon decode <opts>: Decode a jbon value to json."
  putStrLn "jbon analyze <opts>: Analyze a json or jbon value."
  putStrLn "jbon test <opts>: Run tests."
  putStrLn "jbon help <cmd>: Show this help text."
  putStrLn "  Provide the name of a command to show more help about this command and the optional parameters it takes."

testUsage :: IO ()
testUsage = putStrLn "TODO"

decodeUsage :: IO ()
decodeUsage = putStrLn "TODO"

encodeUsage :: IO ()
encodeUsage = putStrLn "TODO"

analyzeUsage :: IO ()
analyzeUsage = putStrLn "TODO"
