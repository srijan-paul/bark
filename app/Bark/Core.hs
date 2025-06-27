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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
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

copyAssets :: Project -> ExceptT ErrorMessage IO ()
copyAssets project = liftIO $ do
  hasAssets <- doesDirectoryExist assetsDir
  when hasAssets $
    copyDirectory assetsDir (outDir </> makeRelative rootDir assetsDir)
  where
    assetsDir = projectAssetsDir project
    rootDir = projectRoot project
    outDir = projectOutDir project

copyCopyDir :: Project -> ExceptT ErrorMessage IO ()
copyCopyDir project = liftIO $ do
  hasCopyDir <- doesDirectoryExist copyDir
  when hasCopyDir $
    copyDirectory copyDir outDir
  where
    copyDir = projectCopyDir project
    outDir = projectOutDir project

buildProjectImpl :: Project -> [Plugin] -> [Post] -> ExceptT ErrorMessage IO ()
buildProjectImpl project plugins posts = do
  -- apply all the pre-processors that modify posts *before* they are compiled.
  compilation <- foldM applyProcessor (newCompilation project posts []) preprocessors
  let processedProject = compilationProject compilation
      processedPosts = compilationPosts compilation

  -- add a "posts" field to the build time data of every post.
  modifiedPosts <- addPostListToPostData processedProject processedPosts
  -- Convert markdown posts to HTML pages.
  pages <- mapM (buildPost project) modifiedPosts
  -- Apply all the post-processors that modify HTML pages after they're built.
  let htmlCompilation = compilation {compilationPages = pages}
  finalCompilation <- foldM applyProcessor htmlCompilation postprocessors

  -- collect all the modified HTML pages, then write to disk
  let processedPages = compilationPages finalCompilation
  mapM_ (liftIO . writeHtmlPage) processedPages

  copyAssets project
  copyCopyDir project
  where
    -- \| returns true if a plugin should be applied *before* a project has been compiled.
    isPrePlugin :: Plugin -> Bool
    isPrePlugin (BeforeBuild _) = True
    isPrePlugin _ = False

    preprocessors = processorOfPlugin <$> filter isPrePlugin plugins
    postprocessors = processorOfPlugin <$> filter (not . isPrePlugin) plugins

    applyProcessor :: Compilation -> Processor () -> ExceptT ErrorMessage IO Compilation
    applyProcessor = flip execStateT

-- | Add a list of posts to the build time data of each post.
addPostListToPostData :: Project -> [Post] -> ExceptT ErrorMessage IO [Post]
addPostListToPostData _ posts' = do
  -- Every gets has an additional data field called `posts`.
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
  buildProjectImpl project plugins posts

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
        result <- runExceptT $ buildProject project plugins
        case result of
          Left errorMessage -> printErrorMessage (T.pack errorMessage)
          Right () -> printWatchMessage time (T.pack relPath)
