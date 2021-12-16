module Bark.Core
  ( buildProject,
    initProject,
  )
where

import CMark (commonmarkToHtml, commonmarkToNode)
import Control.Monad (when)
import Data.HashMap.Strict as HMap (HashMap, empty, fromList, insert, (!))
import Data.Text (Text, isPrefixOf, pack, stripPrefix, stripStart, unpack)
import qualified Data.Text.IO (readFile, writeFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath.Posix (combine, dropFileName, isExtensionOf, replaceDirectory, replaceExtension, (</>))
import Text.Mustache as Mustache (Template (..), ToMustache (toMustache), compileTemplate, substitute)

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

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = combine rootDir "src/content"
  withFilesInDir convert sourceDir
  where
    convert content path = do
      let targetPath = rootDir </> replaceExtension (replaceDirectory path "build") ".html"
      createDirectoryIfMissing True $ dropFileName targetPath
      let htmlContent = commonmarkToHtml [] content
          postData = HMap.fromList [("content", htmlContent)]
          template = compileTemplate "" $ pack "testinnng {{ content }}"
      case template of
        Left _ -> error "Invalid template"
        Right template ->
          let output = substitute template htmlContent
           in Data.Text.IO.writeFile targetPath output
