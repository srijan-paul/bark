module Bark.Core
  ( buildProject,
    initProject,
  )
where

import CMark (commonmarkToHtml, commonmarkToNode)
import Control.Monad (when)
import Data.Text (Text, pack, unpack, isPrefixOf, stripPrefix, stripStart)
import qualified Data.Text.IO (readFile, writeFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath.Posix (combine, dropFileName, replaceDirectory, replaceExtension, (</>), isExtensionOf)

initProject :: FilePath -> IO ()
initProject rootDir = do
  createDirectoryIfMissing True rootDir
  let dirNames = ["src/assets", "src/content", "src/public", "assets"]
   in mapM_ (createDirectoryIfMissing True . combine rootDir) dirNames

withFilesInDir :: (Text -> String -> IO ()) -> FilePath -> IO ()
withFilesInDir _ "" = return ()
withFilesInDir callback path = do
  isFile <- doesFileExist path
  if isFile
    then when (isExtensionOf ".md" path) $ do
      markdown <- Data.Text.IO.readFile path
      callback markdown path
    else do
      -- `path` is itself a directory, and needs to be recursed over
      contents <- listDirectory path
      mapM_
        (withFilesInDir callback . combine path)
        contents

getFrontMatter :: Text -> (String, Text)
getFrontMatter text = 
  if Data.Text.isPrefixOf (pack "---") text then
    ("", pack "x")
  else
    ("", pack "") 

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = combine rootDir "src/content"
  withFilesInDir convert sourceDir
  where
    convert content path = do
      let targetPath = rootDir </> replaceExtension (replaceDirectory path "build") ".html"
      createDirectoryIfMissing True $ dropFileName targetPath
      Data.Text.IO.writeFile targetPath $ commonmarkToHtml [] content
