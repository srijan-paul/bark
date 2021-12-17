module Main where

import Bark.Core (buildProject)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify (watchDir, withManager)
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Exit (exitSuccess)


buildAndLog :: FilePath -> IO ()
buildAndLog projectDir = do
  buildProject projectDir
  print "Built project"

watchProject :: FilePath -> IO ()
watchProject dir =
  let contentPath = dir </> "src" </> "content"
   in withManager $ \mgr -> do
        watchDir mgr contentPath (const True) (\_ -> buildAndLog dir)
        forever $ threadDelay 1000000


showInfo :: IO ()
showInfo = print "Bark v0.1.0. commands available:\n\
  \build: build the current project\n\
  \watch: watch the directory and build whenever changes are detected."

execCommand :: String -> IO ()
execCommand = undefined

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    showInfo
    exitSuccess

  watchProject "scratch"
