module Bark.Core
  ( buildProject,
    initProject,
  )
where

import Bark.FrontMatter (parse)
import CMark (commonmarkToHtml, commonmarkToNode)
import Control.Monad (when)
import Data.HashMap.Strict as HMap (HashMap, empty, fromList, insert, (!))
import Data.Text (Text, isPrefixOf, pack, stripPrefix, stripStart, unpack)
import qualified Data.Text.IO (readFile, writeFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory)
import System.FilePath.Posix (combine, dropFileName, isExtensionOf, replaceDirectory, replaceExtension, takeBaseName, (</>))
import Text.Mustache as Mustache (Template (..), ToMustache (toMustache), compileTemplate, substitute)
import Text.Mustache.Types (Value (..))

initProject :: FilePath -> IO ()
initProject rootDir = do
  createDirectoryIfMissing True rootDir
  let dirNames = ["src/assets", "src/content", "src/public", "assets"]
   in mapM_ (createDirectoryIfMissing True . combine rootDir) dirNames

withFilesInDir :: (Text -> FilePath -> IO ()) -> FilePath -> IO ()
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

readMetaData :: FilePath -> IO Value
readMetaData path = do
  let metaDataPath = replaceExtension path ".meta"
  metaExists <- doesFileExist metaDataPath
  content <-
    if metaExists
      then readFile metaDataPath
      else return ""
  return $ parse content

readTemplate :: FilePath -> FilePath -> Value -> IO Template
readTemplate baseDir path (Object map) = do
  let templateName = case map ! pack "template" of
        String name -> name
        _ -> error "template name must be a string"
      templatePath = baseDir </> "template" </> unpack templateName ++ ".html"

  templateExists <- doesFileExist templatePath
  if not templateExists
    then error $ "template not found: " ++ templatePath
    else do
      templateContent <- Data.Text.IO.readFile templatePath
      case compileTemplate templatePath templateContent of
        Left err -> error $ show err
        Right template -> return template
readTemplate _ _ _ = error "Post metadata must be an object"

convertFile :: String -> Text -> [Char] -> IO ()
convertFile rootDir body mdPath = do
  metaData <- readMetaData mdPath 

  let targetPath = rootDir </> replaceExtension (replaceDirectory mdPath "build") ".html"
      fileBaseName = takeBaseName targetPath
      htmlContent = commonmarkToHtml [] body
      postData = HMap.fromList [("content", String htmlContent), ("meta", metaData)]

  createDirectoryIfMissing True $ dropFileName targetPath
  template <- readTemplate rootDir mdPath metaData

  let output = substitute template postData
   in Data.Text.IO.writeFile targetPath output

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = rootDir </> "src/content"
  withFilesInDir (convertFile rootDir) sourceDir
