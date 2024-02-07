module Main where

import Bark.Core (Project (..), buildProjectWith, watchProjectWith)
import Bark.Processors.SyntaxHighlight (highlightSnippets)
import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath ((</>))

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
  projectPath <- makeAbsolute path
  return
    Project
      { projectRoot = projectPath,
        projectSourceDir = projectPath </> "src",
        projectOutDir = projectPath </> "build",
        projectAssetsDir = projectPath </> "assets",
        projectTemplateDir = projectPath </> "template",
        projectCopyDir = projectPath </> "copy"
      }

doCommand :: Command -> IO ()
doCommand (Build path) = do
  project <- getProject path
  result <- runExceptT $ buildProjectWith [highlightSnippets] project
  case result of
    Left err -> putStrLn $ "Build failed: " ++ err
    Right _ -> return ()
doCommand (Watch path) = do
  putStrLn $ "Watching " ++ path ++ "..."
  project <- getProject path
  watchProjectWith [highlightSnippets] project
doCommand (Init path) = do
  putStrLn $ "Initialized bark project in" ++ path
  mapM_
    (createDirectoryIfMissing True)
    [ path </> "src",
      path </> "build",
      path </> "assets",
      path </> "template",
      path </> "copy"
    ]

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
