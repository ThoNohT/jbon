module Main (main) where

import System.Environment (getArgs)
import Test qualified

-- Json parsers.
main :: IO ()
main = do
  args <- getArgs
  case args of
    "update" : xs -> Test.update $ unwords xs
    ["test"] -> Test.run
    "test" : xs -> Test.runSingleTest $ unwords xs
    _ -> putStrLn "Invalid command."
