module Bark.Core
  ( buildProject,
    initProject,
  )
where

import CMark (commonmarkToHtml)
import Control.Monad (when)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO (readFile, writeFile)
import GHC.Base (build)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath.Posix (combine, dropFileName, replaceDirectory, replaceExtension, takeExtension, (</>))

initProject :: FilePath -> IO ()
initProject rootDir = do
  createDirectoryIfMissing True rootDir
  let dirNames = ["src/assets", "src/content", "src/public", "assets"]
   in mapM_ (createDirectoryIfMissing True . combine rootDir) dirNames

withFilesInDir :: (Text -> String -> IO ()) -> FilePath -> IO ()
withFilesInDir _ "" = return ()
withFilesInDir callback path = do
  isFile <- doesFileExist path
  let isMarkDown = takeExtension path == ".md"
  if isFile
    then when (takeExtension path == ".md") $ do
      markdown <- Data.Text.IO.readFile path
      callback markdown path
    else do
      -- `path` is itself a directory, and needs to be recursed over
      contents <- getDirectoryContents path
      mapM_
        (withFilesInDir callback . combine path)
        (filter (\x -> x /= "." && x /= "..") contents)

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = combine rootDir "src/content"
  withFilesInDir convert sourceDir
  where
    convert content path = do
      let targetPath = rootDir </> replaceExtension (replaceDirectory path "build") ".html"
      createDirectoryIfMissing True $ dropFileName targetPath
      Data.Text.IO.writeFile targetPath $ commonmarkToHtml [] content