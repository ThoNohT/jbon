module Main (main) where

import System.Environment (getArgs)
import Test qualified

-- Json parsers.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> Test.run
    "test" : xs -> Test.runSingleTest $ unwords xs
    "update" : xs -> Test.update $ unwords xs
    "help" : _ -> showUsage
    _ -> do
      putStrLn "Invalid command."
      showUsage

-- | Shows the usage message.
showUsage :: IO ()
showUsage = do
  putStrLn "Usage:"
  putStrLn "jbon test: Run all tests."
  putStrLn "jbon test <test name>: Run the test with the provided name."
  putStrLn "jbon update <test name>: Update the output for he test with the provided name."
  putStrLn "jbon help: Show this help text."
