module Main where

import CMark (commonmarkToHtml)
import Control.Monad (when)
import Data.Text (pack, unpack)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ( combine )

initProject :: FilePath -> IO ()
initProject rootDir = 
  let dirNames = ["src", "src/assets", "src/content", "src/css"]
   in mapM_ (createDirectoryIfMissing True . combine rootDir) dirNames

main :: IO ()
main = initProject "."