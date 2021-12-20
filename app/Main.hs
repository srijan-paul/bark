module Main where

import Bark.Core (buildProject, initProject)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FSNotify (watchDir, withManager)
import System.FilePath ((</>))

buildAndLog :: FilePath -> IO ()
buildAndLog projectDir = do
  buildProject projectDir
  putStrLn "Built project"

watchProject :: FilePath -> IO ()
watchProject dir =
  let contentPath = dir </> "src"
   in withManager $ \mgr -> do
        watchDir mgr contentPath (const True) (\_ -> buildAndLog dir)
        forever $ threadDelay 1000000

showInfo :: IO ()
showInfo =
  putStrLn 
    "Bark v0.1.0. commands available:\n\
    \build: build the current project\n\
    \watch: watch the directory and build whenever changes are detected."

execCommand :: FilePath -> String -> IO ()
execCommand cwd cmd =
  case cmd of
    "build" -> do
      buildProject cwd
      exitSuccess
    "init" -> do
      initProject cwd
      exitSuccess
    "watch" -> watchProject cwd
    _ -> do
      putStrLn $ "unknown command: " ++ cmd

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    showInfo
    exitSuccess
  cwd <- getCurrentDirectory
  execCommand cwd $ head args
