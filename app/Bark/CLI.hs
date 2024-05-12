{-# LANGUAGE OverloadedStrings #-}

module Bark.CLI
  ( parseCommand,
    doCommand,
    Command (..),
    BarkCLI (..),
    defaultCLI,
    builtinPlugins,
  )
where

import Bark.Core
  ( Project (..),
    initProject,
    printErrorMessage,
    printInfoMessage,
    readBarkProject,
    watchProjectWith,
    buildProject,
  )
import Bark.Processors.SyntaxHighlight (highlightPlugin)
import Bark.Types (Plugin (..))
import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory.Internal.Prelude (exitFailure)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

newtype BarkCLI = BarkCLI
  { barkCliPlugins :: [Plugin]
  }

builtinPlugins :: [Plugin]
builtinPlugins = [highlightPlugin]

defaultCLI :: BarkCLI
defaultCLI = BarkCLI builtinPlugins

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
doCommand (BarkCLI plugins) (Build path) = do
  project <- getProject path
  result <- runExceptT $ buildProject project plugins
  case result of
    Left err -> printErrorMessage $ T.pack $ "Build failed: " ++ err
    Right _ -> printInfoMessage "Built project"
doCommand (BarkCLI plugins) (Watch path) = do
  printInfoMessage $ "Watching " <> T.pack path <> "..."
  project <- getProject path
  watchProjectWith plugins project
doCommand _ (Init path) = do
  result <- runExceptT $ initProject path
  case result of
    Left err -> printErrorMessage $ T.pack $ "Failed to initialize project: " ++ err
    Right _ -> printInfoMessage "Project initialized successfully."

parseCommand :: [String] -> Maybe Command
parseCommand args = do
  case (safeHead args, tail args) of
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
    parseCommandArg xs = fromMaybe "." (safeHead xs)
