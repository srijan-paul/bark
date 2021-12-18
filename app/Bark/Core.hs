module Bark.Core
  ( buildProject,
    initProject,
  )
where

import Bark.FrontMatter (parse)
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.HashMap.Strict as HMap (HashMap, fromList, (!))
import Data.List (stripPrefix)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Data.Text.Lazy (toStrict)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FSNotify (withManager)
import System.FilePath.Posix (combine, dropFileName, isExtensionOf, replaceDirectory, replaceExtension, takeBaseName, takeDirectory, (</>))
import Text.Mustache as Mustache (Template (..), ToMustache (toMustache), compileTemplate, substitute)
import Text.Mustache.Types (Value (..))

initProject :: FilePath -> IO ()
initProject rootDir = do
  createDirectoryIfMissing True rootDir
  let dirNames = ["src/assets", "src/content", "src/public", "assets"]
   in mapM_ (createDirectoryIfMissing True . combine rootDir) dirNames

withFilesInDir :: (FilePath -> IO ()) -> FilePath -> IO ()
withFilesInDir _ "" = return ()
withFilesInDir callback path = do
  isFile <- doesFileExist path
  if isFile
    then callback path
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
  let templateName = case map ! T.pack "template" of
        String name -> name
        _ -> error "template name must be a string"
      templatePath = baseDir </> "template" </> T.unpack templateName ++ ".html"

  templateExists <- doesFileExist templatePath
  if not templateExists
    then error $ "template not found: " ++ templatePath
    else do
      templateContent <- TIO.readFile templatePath
      case compileTemplate templatePath templateContent of
        Left err -> error $ show err
        Right template -> return template
readTemplate _ _ _ = error "Post metadata must be an object"

mdPathToRelativeURL:: FilePath -> FilePath -> Maybe FilePath
mdPathToRelativeURL rootDir mdPath =
  stripPrefix (rootDir </> "src" </> "content" ++ "/") mdPath
    >>= \path -> Just $ replaceExtension path ".html"

convertFile :: FilePath -> FilePath -> IO ()
convertFile rootDir mdPath = do
  metaData <- readMetaData mdPath
  body <- TIO.readFile mdPath

  let targetPath = case mdPathToRelativeURL rootDir mdPath of
        Nothing -> error $ "Internal error : Bad file path: " ++ mdPath
        Just path -> rootDir </> "build" </> path 

  let res = commonmarkWith (defaultSyntaxSpec <> gfmExtensions) mdPath body :: (Identity (Either ParseError (Html ())))
      htmlContent = case runIdentity res of
        (Left err) -> error $ show err
        (Right html) -> renderHtml html

  let postData =
        HMap.fromList
          [("content", String $ toStrict htmlContent), ("meta", metaData)]

  createDirectoryIfMissing True $ dropFileName targetPath
  template <- readTemplate rootDir mdPath metaData

  let output = substitute template postData
  TIO.writeFile targetPath output

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = rootDir </> "src"
      contentDir = sourceDir </> "content"

  -- 1. Convert all .md files to corresponding .html files
  let convert filePath = when (isExtensionOf ".md" filePath) $ do
        convertFile rootDir filePath
  withFilesInDir convert contentDir

  -- 2. Copy over the assets and css
  let buildDir = rootDir </> "build"
      copyToBuildDir filePath = do
        fileExists <- doesFileExist filePath
        when fileExists $ do
          case Data.List.stripPrefix sourceDir filePath of
            Nothing -> error "some error ocurred :("
            Just path ->
              let dstPath = buildDir ++ path
                  dstDir = takeDirectory dstPath
               in do
                    createDirectoryIfMissing True dstDir
                    copyFile filePath dstPath

      copyAllToBuildDir dirName = do
        let path = sourceDir </> dirName
        isDir <- doesDirectoryExist path
        isFile <- doesFileExist path
        when (isDir || isFile) $ do
          withFilesInDir copyToBuildDir path

  copyAllToBuildDir "css"
  copyAllToBuildDir "assets"
