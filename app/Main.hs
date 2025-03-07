module Main where

import JSONParser (parseJSON)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      case parseJSON contents of
        Just value -> print value
        Nothing    -> putStrLn "Invalid JSON input"
    _ -> putStrLn "Usage: cabal run jsonad -- <file.json>"
