{-# LANGUAGE OverloadedStrings #-}

module Bark.CLI
  ( parseCommand,
    doCommand,
    Command (..),
    BarkCLI (..),
    defaultCLI,
    builtinProcessors,
  )
where

import Bark.Core
  ( Project (..),
    buildProjectWith,
    initProject,
    printErrorMessage,
    printInfoMessage,
    readBarkProject,
    watchProjectWith,
  )
import Bark.Processors.SyntaxHighlight (highlightSnippets)
import Bark.Types (Processor (..))
import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory.Internal.Prelude (exitFailure)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

newtype BarkCLI = BarkCLI
  { barkCliProcessors :: [Processor]
  }

builtinProcessors :: [Processor]
builtinProcessors = [OnHTML highlightSnippets]

defaultCLI :: BarkCLI
defaultCLI = BarkCLI builtinProcessors

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

doCommand :: BarkCLI -> Command -> IO ()
doCommand (BarkCLI processors) (Build path) = do
  project <- getProject path
  result <- runExceptT $ buildProjectWith processors project
  case result of
    Left err -> printErrorMessage $ T.pack $ "Build failed: " ++ err
    Right _ -> printInfoMessage "Built project"
doCommand (BarkCLI processors) (Watch path) = do
  printInfoMessage $ "Watching " <> T.pack path <> "..."
  project <- getProject path
  watchProjectWith processors project
doCommand _ (Init path) = do
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
