{-# LANGUAGE OverloadedStrings #-}

module Bark.CLI
  ( parseCommand,
    doCommand,
    Command (..),
    BarkCLI (..),
    defaultCLI,
    builtinPlugins,
    showHelp,
    showUsage,
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

-- | Safely get the first element of a list.
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

-- | Load a Bark project from the given path, exiting on failure.
getProject :: FilePath -> IO Project
getProject path = do
  project <- runExceptT $ readBarkProject path
  case project of
    Left err -> do
      printErrorMessage $ T.pack $ "Failed to read project. " ++ err
      exitFailure
    Right p -> return p

-- | Execute a CLI command with the given plugins.
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

-- | Show help information.
showHelp :: IO ()
showHelp = do
  putStrLn "Bark - Static Site Generator"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  bark init [path]   Create a new project (default: current directory)"
  putStrLn "  bark build [path]  Build the site (default: current directory)"
  putStrLn "  bark watch [path]  Build and watch for changes (default: current directory)"
  putStrLn "  bark -h, --help    Show this help"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  bark init my-site  # Create new project in ./my-site"
  putStrLn "  bark build         # Build current directory"
  putStrLn "  bark watch         # Watch current directory and serve on :8080"

-- | Show usage when no command is provided.
showUsage :: IO ()
showUsage = do
  putStrLn "Usage: bark <command> [path]"
  putStrLn "Run 'bark -h' for more information."

-- | Parse command line arguments into a Command.
parseCommand :: [String] -> Maybe Command
parseCommand [] = Nothing
parseCommand ["-h"] = Nothing  -- Will be handled in main
parseCommand ["--help"] = Nothing  -- Will be handled in main
parseCommand (cmd:args) = do
  let path = fromMaybe "." (safeHead args)
  case cmd of
    "build" -> Just (Build path)
    "watch" -> Just (Watch path)
    "init" -> Just (Init path)
    "-h" -> Nothing
    "--help" -> Nothing
    _ -> Nothing
