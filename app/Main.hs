{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bark.Core (Project (..), buildProjectWith, initProject, printErrorMessage, printInfoMessage, readBarkProject, watchProjectWith)
import Bark.Processors.SyntaxHighlight (highlightSnippets)
import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory.Internal.Prelude (exitFailure)
import System.Environment (getArgs)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

data Command
  = Build FilePath
  | Watch FilePath
  | Init FilePath
  deriving (Show)

getProject :: FilePath -> IO Project
getProject path = do
  project <- runExceptT $ readBarkProject path
  case project of
    Left err -> do
      printErrorMessage $ T.pack $ "Failed to read project. " ++ err
      exitFailure
    Right p -> return p

doCommand :: Command -> IO ()
doCommand (Build path) = do
  project <- getProject path
  result <- runExceptT $ buildProjectWith [highlightSnippets] project
  case result of
    Left err -> printErrorMessage $ T.pack $ "Build failed: " ++ err
    Right _ -> printInfoMessage "Built project"
doCommand (Watch path) = do
  printInfoMessage $ T.pack $ "Watching " ++ path ++ "..."
  project <- getProject path
  watchProjectWith [highlightSnippets] project
doCommand (Init path) = do
  result <- runExceptT $ initProject path
  case result of
    Left err -> printErrorMessage $ T.pack $ "Failed to initialize project: " ++ err
    Right _ -> printInfoMessage "Project initialized successfully."

parseCommand :: [String] -> Maybe Command
parseCommand args = do
  case (head' args, tail args) of
    (Just commandStr, rest) -> do
      ctor <- commandCtor commandStr
      return $ ctor (parseCommandArg rest)
    _ -> Nothing
  where
    commandCtor :: String -> Maybe (String -> Command)
    commandCtor "build" = Just Build
    commandCtor "watch" = Just Watch
    commandCtor "init" = Just Init
    commandCtor _ = Nothing

    parseCommandArg :: [String] -> String
    parseCommandArg xs = fromMaybe "." (head' xs)

main :: IO ()
main = do
  maybeCommand <- parseCommand <$> getArgs
  case maybeCommand of
    Just command -> doCommand command
    Nothing -> putStrLn "Usage: bark [build|watch]"
