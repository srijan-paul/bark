{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Bark.CLI (doCommand, parseCommand, defaultCLI)

main :: IO ()
main = do
  maybeCommand <- parseCommand <$> getArgs
  case maybeCommand of
    Just command -> doCommand defaultCLI command
    Nothing -> putStrLn "Unknown command"

