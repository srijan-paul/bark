module Main where

import Bark.Core (buildProject)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify (watchDir, withManager)
import System.FilePath ((</>))


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

main :: IO ()
main = do
  watchProject "scratch"