module Bark.Core
  ( buildProject,
    initProject,
  )
where

import Bark.FrontMatter (parseString)
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (runIdentity))
import Data.HashMap.Strict as HashMap (fromList, (!))
import Data.List (stripPrefix)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Data.Text.Lazy (toStrict)
import qualified Data.Vector as Vec
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath.Posix
  ( combine,
    dropExtension,
    dropFileName,
    isExtensionOf,
    replaceExtension,
    takeBaseName,
    takeDirectory,
    (</>),
  )
import Text.Mustache as Mustache (Template (..), compileTemplate, substitute)
import Text.Mustache.Types (Value (..))

initProject :: FilePath -> IO ()
initProject rootDir = do
  createDirectoryIfMissing True rootDir
  let dirNames =
        [ "src/assets",
          "src/content",
          "src/css",
          "template",
          "src/static",
          "src/copy-over"
        ]
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
      else error $ "Metadata does not exist for file " ++ path
  case parseString content of
    Left errorMsg -> error $ "Error while reading " ++ path ++ "\n" ++ errorMsg
    Right value -> return value

readTemplate :: FilePath -> FilePath -> Value -> IO Template
readTemplate baseDir path (Object metadata) = do
  let templateName = case metadata ! T.pack "template" of
        String tName -> tName
        _ -> error "template name must be a string"
      templatePath = baseDir </> "template" </> T.unpack templateName ++ ".mustache"

  templateExists <- doesFileExist templatePath
  if not templateExists
    then error $ "template not found: " ++ templatePath
    else do
      templateContent <- TIO.readFile templatePath
      case compileTemplate templatePath templateContent of
        Left err -> error $ show err
        Right template -> return template
readTemplate _ _ _ = error "Post metadata must be an object"

-- | Given the project's root directory (`rootDir`) and the absolute path of a
--  markdown file (`mdPath`), returns a string representing the URL slug.
--  Note that the HTML output of markdown files named anything but `index` will be put in a file
--  called `index.html` inside a folder having the same name as the markdown file.
--
-- >>> mdPathToRelativeURL "foo" "foo/src/content/bar/file.md"
-- Just "bar/file/index.html"
--  >>> mdPathToRelativeURL "foo" "foo/src/content/bar/index.md"
-- Just "bar/index.html"
mdPathToRelativeURL :: FilePath -> FilePath -> Maybe FilePath
mdPathToRelativeURL rootDir mdPath =
  stripPrefix (rootDir </> "src" </> "content" ++ "/") mdPath
    >>= \path ->
      if takeBaseName path /= "index"
        then Just $ dropExtension path </> "index.html"
        else Just $ replaceExtension path  ".html"

convertFile :: Value -> FilePath -> FilePath -> IO ()
convertFile allPostsMeta rootDir mdPath = do
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
        HashMap.fromList
          [ ("content", String $ toStrict htmlContent),
            ("meta", metaData),
            ("posts", allPostsMeta)
          ]

  createDirectoryIfMissing True $ dropFileName targetPath
  template <- readTemplate rootDir mdPath metaData

  let output = substitute template postData
  TIO.writeFile targetPath output

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = rootDir </> "src"
      contentDir = sourceDir </> "content"

  -- First we prepare the metadata of all the files such that
  -- it can be made available to the the mustache template
  allFiles <- getFilesRecursive contentDir
  let mdFilePaths = filter (isExtensionOf ".md") allFiles
      allPostsMeta = Vec.empty :: Vec.Vector Value
      metaOf mdPath = do
        metaData <- readMetaData mdPath
        let (postName, meta) = (takeBaseName mdPath, metaData)
            obj =
              HashMap.fromList
                [ (T.pack "name", String $ T.pack postName),
                  (T.pack "data", meta)
                ]
        return $ Object obj

  metaList <- mapM metaOf mdFilePaths
  let postsMeta = Array $ Vec.fromList metaList

  -- 1. Convert all .md files to corresponding .html files
  let convert filePath = when (isExtensionOf ".md" filePath) $ do
        convertFile postsMeta rootDir filePath
  for_ mdFilePaths convert

  -- 2. Copy over the assets and css
  let buildDir = rootDir </> "build"
      copyToBuildDir filePath = do
        fileExists <- doesFileExist filePath
        when fileExists $ do
          case Data.List.stripPrefix sourceDir filePath of
            Nothing -> error "an unknown error occured, please file a bug report."
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
  copyAllToBuildDir "static"

  -- copy over everything in the `copy` directory
  let copyDirPath = sourceDir </> "copy"
  isDir <- doesDirectoryExist copyDirPath

  when isDir $ do
    let copyToBuildRoot path = do
          -- srcPath = path of the file to copy relative to `copy` directory.
          -- dstPath = absolute path of the file to copy
          let srcPath = stripPrefix (copyDirPath ++ "/") path
              dstPath = fmap (buildDir </>) srcPath
          case (srcPath, dstPath) of
            (Just src, Just dst) -> do
              createDirectoryIfMissing True $ dropFileName dst
              copyFile path dst
            _ -> return ()
    filesToCopyOver <- getFilesRecursive copyDirPath
    mapM_ copyToBuildRoot filesToCopyOver
