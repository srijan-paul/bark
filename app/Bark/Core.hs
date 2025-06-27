{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Bark.Core
  ( getPostFromMdfile,
    Project (..),
    Post (..),
    postToHtml,
    buildPost,
    initProject,
    buildProject,
    watchProjectWith,
    defaultConfig,
    readBarkConfig,
    readBarkProject,
    printErrorMessage,
    printInfoMessage,
    urlFromMdPath,
    template2Html,
    templateFromPath,
    module System.FilePath,
  )
where

import Bark.FrontMatter (PostFrontMatter (..), parseFrontMatter)
import Bark.Internal.IOUtil (ErrorMessage, copyDirectory, tryReadFileBS, tryReadFileT)
import Bark.Types
  ( Compilation (..),
    HTMLPage (..),
    Plugin (..),
    Post (..),
    Processor,
    Project (..),
    ProjectConfig (..),
    htmlFromPost,
    newCompilation,
    processorOfPlugin,
  )
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Arrow (ArrowChoice (left))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (foldM, forever, unless, when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (execStateT)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Char8 as Char8 
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.List (findIndex, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import System.Directory.Recursive (getFilesRecursive)
import qualified System.FSNotify as FS
import System.FilePath
  ( dropExtension,
    makeRelative,
    normalise,
    replaceExtension,
    takeBaseName,
    takeDirectory,
    takeExtension,
    (</>),
  )
import qualified Text.Colour as Color
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Mustache

-- | Safely get the last element of a list.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

defaultConfig :: T.Text
defaultConfig =
  "# All paths are relative to the project root directory.\n\
  \source: src\n\
  \out: build\n\
  \assets: assets\n\
  \template: template\n\
  \copy: copy\n"

-- | Initialize a new Bark project at the given path with default structure and config.
initProject :: FilePath -> ExceptT ErrorMessage IO Project
initProject path = do
  liftIO $ createDirectoryIfMissing True path
  liftIO $ TIO.writeFile (path </> "bark.yml") defaultConfig
  project <- readBarkProject path
  liftIO $
    mapM_
      (createDirectoryIfMissing True)
      [ projectSourceDir project,
        projectOutDir project,
        projectAssetsDir project,
        projectTemplateDir project,
        projectCopyDir project
      ]

  return project

-- | Read and parse the bark.yml configuration file from the given project path.
readBarkConfig :: FilePath -> ExceptT ErrorMessage IO ProjectConfig
readBarkConfig path = do
  result <- liftIO $ decodeFileEither path
  liftEither $ left prettyPrintParseException result

-- | Read a bark project from a config file.
readBarkProject :: FilePath -> ExceptT ErrorMessage IO Project
readBarkProject path = do
  rootPath <- liftIO $ makeAbsolute path
  config <- readBarkConfig (rootPath </> "bark.yml")
  return $
    Project
      { projectRoot = rootPath,
        projectSourceDir = rootPath </> configSourceDir config,
        projectOutDir = configOutDir config,
        projectAssetsDir = configAssetsDir config,
        projectTemplateDir = configTemplateDir config,
        projectCopyDir = configCopyDir config
      }

-- | From markdown file path, get the post's URL.
-- ### Examples:
--
-- > urlFromMdPath "project_root/hello-world.md" = "hello-world/index.html"
--
-- > urlFromMdPath "project_root/blog/hello-world.md" = "blog/hello-world/index.html"
--
-- > urlFromMdPath "project_root/blog/index.md" = "blog/index.html"
urlFromMdPath :: Project -> FilePath -> FilePath
urlFromMdPath Project {projectSourceDir = sourceDir} mdFilePath
  | takeBaseName mdFilePath == "index" = replaceExtension (makeRelative sourceDir mdFilePath) ".html"
  | otherwise = makeRelative sourceDir (dropExtension mdFilePath </> "index.html")

-- | Parse a post belonging to a project from a markdown file path.
getPostFromMdfile :: Project -> FilePath -> ExceptT ErrorMessage IO Post
getPostFromMdfile project filePath = do
  content <- tryReadFileBS filePath
  frontmatter <- liftEither $ parseFrontMatter filePath content
  let url = urlFromMdPath project filePath
      dstPath = projectOutDir project </> url
  return $
    Post
      { postPath = filePath,
        postDstPath = dstPath,
        postFrontMatter = frontmatter,
        postContent = T.decodeUtf8 content,
        postUrl = url,
        postData = HM.singleton "url" (Mustache.String $ T.pack url)
      }

-- | Convert markdown content to HTML.
md2Html :: FilePath -> T.Text -> Either ErrorMessage T.Text
md2Html path contents =
  bimap show (TL.toStrict . renderHtml) (runIdentity parseResult)
  where
    parseResult =
      commonmarkWith
        (defaultSyntaxSpec <> gfmExtensions)
        path
        contents ::
        (Identity (Either ParseError (Html ())))

-- | Strip the frontmatter from a markdown file contents.
stripFrontMatter :: T.Text -> T.Text
stripFrontMatter = T.unlines . skipFrontMatter . T.lines
  where
    isDelim = T.all (== '-')
    isEmpty = T.all Char.isSpace

    skipFrontMatter :: [T.Text] -> [T.Text]
    skipFrontMatter [] = []
    skipFrontMatter (ln : lns)
      | isEmpty ln = skipFrontMatter lns
      | isDelim ln =
          case findIndex isDelim lns of
            Just i -> drop (i + 1) lns
            Nothing -> lns
      | otherwise = lns

-- | Convert a post's markdown content to an HTML string.
postToHtml :: Post -> Either ErrorMessage T.Text
postToHtml Post {postPath = path, postContent = content} =
  md2Html
    path
    (stripFrontMatter content)

-- | Load a mustache template from a file path.
templateFromPath :: FilePath -> ExceptT ErrorMessage IO Mustache.Template
templateFromPath path = do
  templateContent <- tryReadFileT path
  let template = Mustache.compileTemplate path templateContent
  liftEither $ left show template

loadTemplate :: Project -> Post -> ExceptT ErrorMessage IO Mustache.Template
loadTemplate project post = do
  let templateName = T.unpack $ fmTemplate (postFrontMatter post)
      templatePath = projectTemplateDir project </> templateName ++ ".mustache"
  templateFromPath templatePath

-- | Convert a mustache template to HTML text by substituting the given data.
template2Html :: FilePath -> Mustache.Value -> ExceptT ErrorMessage IO T.Text
template2Html templatePath data' = do
  template <- templateFromPath templatePath
  let output = Mustache.substitute template data'
  return output

buildPost :: Project -> Post -> ExceptT ErrorMessage IO HTMLPage
buildPost project post = do
  postHtml <- liftEither $ postToHtml post
  template <- loadTemplate project post
  let metadata = fmMetaData (postFrontMatter post)
      buildData =
        Mustache.Object $
          HM.fromList
            [ ("content", Mustache.String postHtml),
              ("meta", Mustache.Object metadata)
            ]
            <> postData post
      output = Mustache.substitute template buildData
  return $ htmlFromPost post output

writeHtmlPage :: HTMLPage -> IO ()
writeHtmlPage (HTMLPage _ content outPath) = do
  createDirectoryIfMissing True $ takeDirectory outPath
  TIO.writeFile outPath content

-- | Get a list of all posts in the project.
getPosts :: Project -> ExceptT ErrorMessage IO [Post]
getPosts project = do
  files <- liftIO $ getFilesRecursive (projectSourceDir project)
  liftIO $ createDirectoryIfMissing True (projectOutDir project)
  let markdownFiles = filter ((`elem` [".markdown", ".md"]) . takeExtension) files
  mapM (getPostFromMdfile project) markdownFiles

-- | Copy all files from the project's assets directory to the output directory.
copyAssets :: Project -> IO ()
copyAssets project = do
  hasAssets <- doesDirectoryExist assetsDir
  when hasAssets $
    copyDirectory assetsDir (outDir </> makeRelative rootDir assetsDir)
  where
    assetsDir = projectAssetsDir project
    rootDir = projectRoot project
    outDir = projectOutDir project

-- | Copy all files from the project's copy directory to the output directory.
copyStaticFiles :: Project -> IO ()
copyStaticFiles project = do
  hasCopyDir <- doesDirectoryExist copyDir
  when hasCopyDir $
    copyDirectory copyDir outDir
  where
    copyDir = projectCopyDir project
    outDir = projectOutDir project

-- | Copy a single asset file incrementally
copySingleAsset :: Project -> FilePath -> ExceptT ErrorMessage IO ()
copySingleAsset project assetPath = liftIO $ do
  let rootDir = projectRoot project
      outDir = projectOutDir project  
      relPath = makeRelative rootDir assetPath
      destPath = outDir </> relPath
  createDirectoryIfMissing True $ takeDirectory destPath
  copyFile assetPath destPath

-- | Copy a single file from copy directory incrementally  
copySingleCopyFile :: Project -> FilePath -> ExceptT ErrorMessage IO ()
copySingleCopyFile project copyPath = liftIO $ do
  let copyDir = projectCopyDir project
      outDir = projectOutDir project
      relPath = makeRelative copyDir copyPath
      destPath = outDir </> relPath
  createDirectoryIfMissing True $ takeDirectory destPath
  copyFile copyPath destPath

-- | Apply preprocessing plugins to posts before HTML conversion.
applyPreprocessingPlugins :: Project -> [Plugin] -> [Post] -> ExceptT ErrorMessage IO [Post]
applyPreprocessingPlugins project plugins posts = do
  let preprocessors = processorOfPlugin <$> filter isPrePlugin plugins
  compilation <- foldM applyProcessor (newCompilation project posts []) preprocessors
  return $ compilationPosts compilation
  where
    isPrePlugin (BeforeBuild _) = True
    isPrePlugin _ = False

-- | Convert posts to HTML pages and apply postprocessing plugins.
buildAndProcessPages :: Project -> [Plugin] -> [Post] -> ExceptT ErrorMessage IO [HTMLPage]
buildAndProcessPages project plugins posts = do
  let postprocessors = processorOfPlugin <$> filter (not . isPrePlugin) plugins
  pages <- mapM (buildPost project) posts
  let htmlCompilation = newCompilation project [] pages
  finalCompilation <- foldM applyProcessor htmlCompilation postprocessors
  return $ compilationPages finalCompilation
  where
    isPrePlugin (BeforeBuild _) = True
    isPrePlugin _ = False

-- | Copy all project assets and static files to output directory.
copyProjectFiles :: Project -> ExceptT ErrorMessage IO ()
copyProjectFiles project = liftIO $ do
  copyAssets project
  copyStaticFiles project

-- | Core build pipeline: applies plugins, converts posts to HTML, and copies assets.
buildPipeline :: Project -> [Plugin] -> [Post] -> ExceptT ErrorMessage IO ()
buildPipeline project plugins posts = do
  processedPosts <- applyPreprocessingPlugins project plugins posts
  enrichedPosts <- enrichPostsWithPostsList project processedPosts
  finalPages <- buildAndProcessPages project plugins enrichedPosts
  mapM_ (liftIO . writeHtmlPage) finalPages
  copyProjectFiles project

applyProcessor :: Compilation -> Processor () -> ExceptT ErrorMessage IO Compilation
applyProcessor = flip execStateT

-- | Inject a 'posts' array into each post's template data for cross-references.
enrichPostsWithPostsList :: Project -> [Post] -> ExceptT ErrorMessage IO [Post]
enrichPostsWithPostsList _ posts' = do
  -- Every post has an additional data field called `posts`.
  -- It is an array of all posts in the project.
  let allPosts = Mustache.Array $ Vector.fromList $ map getPostData posts'
      posts = map (\p -> p {postData = HM.insert "posts" allPosts (postData p)}) posts'
  return posts
  where
    getPostData :: Post -> Mustache.Value
    getPostData post =
      let meta = Mustache.Object $ fmMetaData $ postFrontMatter post
       in Mustache.Object $ HM.insert "meta" meta (postData post)

-- | Build a bark project using the given list of plugins.
buildProject :: Project -> [Plugin] -> ExceptT ErrorMessage IO ()
buildProject project plugins = do
  posts <- getPosts project
  buildPipeline project plugins posts

-- | Build a single post incrementally by file path
buildSinglePost :: Project -> [Plugin] -> FilePath -> ExceptT ErrorMessage IO ()
buildSinglePost project plugins filePath = do
  post <- getPostFromMdfile project filePath
  -- For simplicity, we apply plugins to just this post
  -- Note: This is a simplified approach - ideally we'd need all posts for cross-references
  compilation <- foldM applyProcessor (newCompilation project [post] []) preprocessors
  let processedPosts = compilationPosts compilation
  case processedPosts of
    [processedPost] -> do
      -- Add posts list data (empty for incremental builds to keep it simple)
      let postWithData = processedPost { postData = postData processedPost <> HM.fromList [("posts", Mustache.Array mempty)] }
      page <- buildPost project postWithData
      finalPage <- foldM (\p processor -> do
        let htmlComp = newCompilation project [] [p]
        result <- applyProcessor htmlComp processor  
        case compilationPages result of
          [finalP] -> return finalP
          _ -> return p
        ) page postprocessors
      liftIO $ writeHtmlPage finalPage
    _ -> liftEither $ Left "Unexpected number of processed posts"
  where
    isPrePlugin :: Plugin -> Bool
    isPrePlugin (BeforeBuild _) = True
    isPrePlugin _ = False
    
    preprocessors = processorOfPlugin <$> filter isPrePlugin plugins
    postprocessors = processorOfPlugin <$> filter (not . isPrePlugin) plugins

printWatchMessage :: T.Text -> T.Text -> IO ()
printWatchMessage time filePath = do
  let coloredTime = Color.fore Color.blue (Color.chunk time)
      coloredFilePath = Color.fore Color.yellow (Color.chunk filePath)
      txt =
        [ Color.chunk "[",
          coloredTime,
          Color.chunk "] modified ",
          coloredFilePath,
          Color.chunk " - Rebuilt project."
        ]
   in putStrLn $ T.unpack $ Color.renderChunksText Color.With8Colours txt

printInfoMessage :: T.Text -> IO ()
printInfoMessage message = do
  let txt =
        [ Color.back Color.brightBlue $ Color.fore Color.black (Color.chunk "[INFO]"),
          Color.chunk " ",
          Color.fore Color.green (Color.chunk message)
        ]
   in putStrLn $ T.unpack (Color.renderChunksText Color.With8Colours txt)

printErrorMessage :: T.Text -> IO ()
printErrorMessage errorMessage = do
  let message =
        [ Color.fore Color.red (Color.chunk "[ERROR]"),
          Color.chunk " ",
          Color.fore Color.yellow (Color.chunk errorMessage)
        ]
   in putStrLn $ T.unpack (Color.renderChunksText Color.With8Colours message)

redirectDirectoryToIndex :: Wai.Middleware
redirectDirectoryToIndex app req sendResp =
  let path = Wai.rawPathInfo req
   in if Char8.last path == '/'
        then
          sendResp $
            Wai.responseLBS
              Http.status301
              [("Location", path <> "index.html")]
              ""
        else app req sendResp

-- | Watch a project for file changes and rebuild incrementally. Also serves the site on port 8080.
watchProjectWith :: [Plugin] -> Project -> IO ()
watchProjectWith plugins project = FS.withManager $ \mgr -> do
  _ <- FS.watchTree mgr sourceDir filterEvent callback
  _ <- FS.watchTree mgr assetsDir filterEvent callback
  _ <- FS.watchTree mgr templateDir filterEvent callback
  _ <- FS.watchTree mgr copyDir filterEvent callback

  let serveSettings = Wai.defaultWebAppSettings (projectOutDir project)
  let app = redirectDirectoryToIndex (Wai.staticApp serveSettings)
  -- TODO: allow user to set the port.
  _ <- forkIO $ Warp.run 8080 app
  printInfoMessage "Listening on port :8080"
  forever $ threadDelay 1_000_000
  where
    sourceDir = projectSourceDir project
    assetsDir = projectAssetsDir project
    templateDir = projectTemplateDir project
    copyDir = projectCopyDir project

    filterEvent :: FS.Event -> Bool
    filterEvent (FS.Modified filePath _ _) =
      -- ignore file paths ending in "~", they're usually temporary files made by editors and such.
      safeLast filePath /= Just '~'
    filterEvent (FS.Added {}) = True
    filterEvent (FS.Removed {}) = True
    filterEvent _ = False

    callback event = do
      let path = normalise $ FS.eventPath event
          relPath = makeRelative (projectRoot project) path
          time = T.pack $ show $ FS.eventTime event
          isInBuildDir = projectOutDir project `isPrefixOf` path

      unless isInBuildDir $ do
        result <- runExceptT $ handleFileChange path
        case result of
          Left errorMessage -> printErrorMessage (T.pack errorMessage)
          Right () -> printWatchMessage time (T.pack relPath)
      where
        handleFileChange filePath
          | sourceDir `isPrefixOf` filePath && takeExtension filePath == ".md" = 
              buildSinglePost project plugins filePath
          | assetsDir `isPrefixOf` filePath = 
              copySingleAsset project filePath
          | copyDir `isPrefixOf` filePath = 
              copySingleCopyFile project filePath
          | templateDir `isPrefixOf` filePath = 
              buildProject project plugins  -- Template changes affect all posts
          | otherwise = 
              buildProject project plugins  -- Fallback to full rebuild
