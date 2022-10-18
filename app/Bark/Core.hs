module Bark.Core
  ( buildProject,
    initProject,
  )
where

import Bark.FrontMatter (parseString)
import Bark.Internal.IOUtil (Result, tryReadFileT, withFilesInDir)
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Monad (when)
import Data.Functor.Identity (Identity, runIdentity)
import Data.HashMap.Strict as HashMap (fromList, (!))
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as TIO (writeFile)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
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

type ErrorMsg = String

initProject :: FilePath -> IO ()
initProject rootDir = do
  createDirectoryIfMissing True rootDir
  let dirNames =
        [ "src" </> "assets",
          "src" </> "content",
          "src" </> "css",
          "template",
          "src" </> "static",
          "src" </> "copy-over"
        ]
   in mapM_ (createDirectoryIfMissing True . combine rootDir) dirNames

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

loadTemplateFrompath :: FilePath -> IO (Result Template)
loadTemplateFrompath templatePath = do
  templateContent <- tryReadFileT templatePath
  return $ templateContent >>= compileBarkTemplate templatePath

compileBarkTemplate :: FilePath -> Text -> Result Mustache.Template
compileBarkTemplate path body = case Mustache.compileTemplate path body of
  Left err -> Left $ show err
  Right template -> Right template

readTemplate :: FilePath -> Value -> IO (Result Template)
readTemplate rootDir (Object metadata) = do
  -- name of the template specified in the metadata
  let templateName_v = metadata ! T.pack "template"
  case templateName_v of
    String templateName ->
      let templatePath = rootDir </> "template" </> T.unpack templateName ++ ".mustache"
       in loadTemplateFrompath templatePath
    other -> return $ Left $ "Template name not a string (" ++ show other ++ ")"
readTemplate _ _ = return $ Left "Post metadata must be an object"

-- | Given the project's root directory (`rootDir`) and the absolute path of a
-- | markdown file (`mdPath`), returns a string representing the URL slug.
-- | Note that the HTML output of markdown files named anything but `index` will be put in a file
-- | called `index.html` inside a folder having the same name as the markdown file.
--
-- >>> mdPathToRelativeURL "foo" "foo/src/content/bar/file.md"
-- Just "bar/file/index.html"
--  >>> mdPathToRelativeURL "foo" "foo/src/content/bar/index.md"
-- Just "bar/index.html"
mdPathToRelativeURL :: FilePath -> FilePath -> Maybe FilePath
mdPathToRelativeURL rootDir mdPath =
  stripPrefix (rootDir </> "src" </> "content" ++ "/") mdPath
    >>= \path ->
      Just $
        if takeBaseName path /= "index"
          then dropExtension path </> "index.html"
          else replaceExtension path ".html"

-- | Convert the markdown content passed as argument to HTML.
mdToHTML :: FilePath -> Text -> Result TL.Text
mdToHTML path contents =
  let res = commonmarkWith (defaultSyntaxSpec <> gfmExtensions) path contents :: (Identity (Either ParseError (Html ())))
   in case runIdentity res of
        (Left err) -> Left $ show err
        (Right html) -> Right $ renderHtml html

mdFileToHTML :: FilePath -> IO (Result TL.Text)
mdFileToHTML path = do
  body <- tryReadFileT path
  return $
    either (const $ Left ("Could not read file: " ++ path)) (mdToHTML path) body

-- | Converts a markdown file to its corresponding HTML
-- | document, and then writes it to the appropriate location
-- | in the filesystem
convertFile :: Value -> FilePath -> FilePath -> IO ()
convertFile allPostsMeta rootDir mdPath = do
  metaData <- readMetaData mdPath

  let targetPath = case mdPathToRelativeURL rootDir mdPath of
        Nothing -> error $ "Internal error - Bad file path: " ++ mdPath
        Just path -> rootDir </> "build" </> path

  errorOrHtml <- mdFileToHTML mdPath

  let htmlContent = case errorOrHtml of
        Left err -> error err
        Right html -> html
      postData =
        HashMap.fromList
          [ ("content", String $ TL.toStrict htmlContent),
            ("meta", metaData),
            ("posts", allPostsMeta)
          ]

  createDirectoryIfMissing True $ dropFileName targetPath
  template <- readTemplate rootDir metaData
  case template of
    Left err -> error err
    Right tem -> TIO.writeFile targetPath $ substitute tem postData

-- | Returns a list containing the paths to all markdown files in a directory, and its subdirectories
getMdFilesRecursive :: FilePath -> IO [FilePath]
getMdFilesRecursive dirPath = do
  allFiles <- getFilesRecursive dirPath
  return $ filter (isExtensionOf ".md") allFiles

-- | Given a list of paths to `.md` files, return the corresponding metadata, in the same order.
-- | It is assumed that a file at path "foo/bar/baz.md", will have its metadata at "foo/bar/baz.meta".
getMetaDataOfPosts :: [FilePath] -> IO Value
getMetaDataOfPosts mdPaths = do
  metaList <- mapM readMetaDataOf mdPaths
  return (Array $ Vec.fromList metaList)
  where
    readMetaDataOf :: FilePath -> IO Value
    readMetaDataOf mdPath = do
      metaData <- readMetaData mdPath
      let (postName, meta) = (String $ T.pack $ takeBaseName mdPath, metaData)
      return
        (Object $ HashMap.fromList [(T.pack "name", postName), (T.pack "data", meta)])

buildProject :: FilePath -> IO ()
buildProject rootDir = do
  let sourceDir = rootDir </> "src"
      contentDir = sourceDir </> "content"
      outDir = rootDir </> "build"

  -- First, we prepare the global object containing metadata of all the
  -- files so that it can be made available to the the mustache template
  mdFilePaths <- getMdFilesRecursive contentDir
  postsMeta <- getMetaDataOfPosts mdFilePaths

  -- 1. Convert all .md files to corresponding .html files
  mapM_ (convertFile postsMeta rootDir) mdFilePaths

  -- 2. Copy over the assets and css

  -- copy the file present at `filePath` to its corresponding destination path
  -- inside the build directory.
  -- "/home/project/src/assets/foo/bar.css" -> "/home/project/build/assets/foo/bar.css"
  let copyToBuildDir filePath = do
        fileExists <- doesFileExist filePath
        when fileExists $ do
          -- Convert an absolute path to a path relative to the `content` directory
          -- /home/injuly/project/src/content/foo/bar.css -> foo/bar.css
          let relativePath = stripPrefix sourceDir filePath
          case relativePath of
            Nothing -> error "An unknown error occured, please file a bug report."
            Just path -> do
              -- `path` begins with a `/` so we can't use `</>` here.
              let dstPath = outDir ++ path
                  dstDir = takeDirectory dstPath
              createDirectoryIfMissing True dstDir
              copyFile filePath dstPath

      -- recursively copy all files in `dirName` to their corresponding places
      -- in the build directory
      copyContentsToBuildDir dirName = do
        let path = sourceDir </> dirName
        isDir <- doesDirectoryExist path
        isFile <- doesFileExist path
        when (isDir || isFile) $ do
          withFilesInDir copyToBuildDir path

  copyContentsToBuildDir "css"
  copyContentsToBuildDir "assets"
  copyContentsToBuildDir "static"

  -- copy over everything in the `copy-over` directory
  let copyDirPath = sourceDir </> "copy"
  isDir <- doesDirectoryExist copyDirPath

  when isDir $ do
    let copyToBuildRoot path = do
          -- srcPath = path of the file to copy relative to `copy` directory.
          -- dstPath = absolute path of the file to copy
          let srcPath = stripPrefix (copyDirPath ++ "/") path
              dstPath = fmap (outDir </>) srcPath
          case (srcPath, dstPath) of
            (Just src, Just dst) -> do
              createDirectoryIfMissing True $ dropFileName dst
              copyFile path dst
            _ -> return ()
    filesToCopyOver <- getFilesRecursive copyDirPath
    mapM_ copyToBuildRoot filesToCopyOver
