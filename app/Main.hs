{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Bark.CLI (doCommand, parseCommand, defaultCLI, showHelp, showUsage)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> showUsage
    ["-h"] -> showHelp
    ["--help"] -> showHelp
    _ -> do
      let maybeCommand = parseCommand args
      case maybeCommand of
        Just command -> doCommand defaultCLI command
        Nothing -> do
          putStrLn $ "Unknown command: " ++ unwords args
          putStrLn "Run 'bark -h' for help."

