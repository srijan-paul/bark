{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Bark.Core
  ( getPostFromMdfile,
    Project (..),
    Post (..),
    postToHtml,
    buildPost,
    buildProject,
    buildProjectWith,
    watchProjectWith,
  )
where

import Bark.FrontMatter (PostFrontMatter (..), parseFrontMatter)
import Bark.Internal.IOUtil (ErrorMessage, copyDirectory, tryReadFileBS, tryReadFileT)
import Bark.Types (HTMLPage (..), Post (..), Postprocessor, Project (..))
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Arrow (ArrowChoice (left))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import Control.Monad.Except (ExceptT, MonadIO (liftIO), foldM, liftEither, runExceptT, when)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.List (findIndex, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
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
        postOtherData = []
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

loadTemplate :: Project -> Post -> ExceptT ErrorMessage IO Mustache.Template
loadTemplate project post = do
  let templateName = T.unpack $ fmTemplate (postFrontMatter post)
      templatePath = projectTemplateDir project </> templateName ++ ".mustache"
  templateContent <- tryReadFileT templatePath
  let template = Mustache.compileTemplate templatePath templateContent
  liftEither $ left show template

buildPost :: Project -> Post -> ExceptT ErrorMessage IO HTMLPage
buildPost project post = do
  postHtml <- liftEither $ postToHtml post
  template <- loadTemplate project post
  let metadata = fmMetaData (postFrontMatter post)
      postData =
        Mustache.Object $
          HM.fromList $
            [ ("content", Mustache.String postHtml),
              ("meta", Mustache.Object metadata)
            ]
              ++ postOtherData post
      output = Mustache.substitute template postData
  return $ HTMLPage post output

writeHtmlPage :: HTMLPage -> ExceptT ErrorMessage IO ()
writeHtmlPage (HTMLPage post content) = do
  let outPath = postDstPath post
  liftIO $ createDirectoryIfMissing True $ takeDirectory outPath
  liftIO $ TIO.writeFile outPath content

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
    copyDirectory copyDir (outDir </> makeRelative rootDir copyDir)
  where
    copyDir = projectCopyDir project
    rootDir = projectRoot project
    outDir = projectOutDir project

buildProjectImpl :: Project -> [Postprocessor] -> [Post] -> ExceptT ErrorMessage IO ()
buildProjectImpl project postprocessors posts = do
  -- Convert markdown posts to HTML pages.
  pages <- mapM (buildPost project) posts
  -- apply any post compilation processors (e.g. syntax highlighting, etc.)
  processedPages <- mapM applyHtmlProcessors pages
  -- write the processed pages to disk
  mapM_ writeHtmlPage processedPages
  -- copy assets directory to the output directory
  copyAssets project
  -- copy the copy directory to the output directory
  copyCopyDir project
  where
    applyHtmlProcessors :: HTMLPage -> ExceptT ErrorMessage IO HTMLPage
    applyHtmlProcessors page = foldM (\p f -> f project p) page postprocessors

addPostListToMeta :: Project -> [Post] -> ExceptT ErrorMessage IO [Post]
addPostListToMeta (Project {projectOutDir = outDir}) posts' = do
  -- Every post has an additional data field called `posts`.
  -- It is an array of all posts in the project.
  let allPosts = Mustache.Array $ Vector.fromList $ map getPostData posts'
      posts = map (\p -> p {postOtherData = [("posts", allPosts)]}) posts'
  return posts
  where
    getPostData :: Post -> Mustache.Value
    getPostData post =
      let meta = Mustache.Object $ fmMetaData $ postFrontMatter post
          relativeUrl = makeRelative outDir (postDstPath post)
          dstPath = Mustache.String $ T.pack relativeUrl
       in Mustache.Object $ HM.fromList [("meta", meta), ("dstPath", dstPath)]

-- | Build a bark project
buildProject :: Project -> ExceptT ErrorMessage IO ()
buildProject project = do
  posts <- getPosts project >>= addPostListToMeta project
  buildProjectImpl project [] posts

buildProjectWith :: [Postprocessor] -> Project -> ExceptT ErrorMessage IO ()
buildProjectWith postprocessors project = do
  posts <- getPosts project >>= addPostListToMeta project
  buildProjectImpl project postprocessors posts

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

printErrorMessage :: T.Text -> IO ()
printErrorMessage errorMessage = do
  let message =
        [ Color.chunk "[",
          Color.fore Color.blue (Color.chunk "ERROR"),
          Color.chunk "] ",
          Color.fore Color.red (Color.chunk errorMessage)
        ]
   in putStrLn $ T.unpack (Color.renderChunksText Color.With8Colours message)

watchProjectWith :: [Postprocessor] -> Project -> IO ()
watchProjectWith processors project = FS.withManager $ \mgr -> do
  _ <- FS.watchTree mgr sourceDir filterEvent callback
  _ <- FS.watchTree mgr assetsDir filterEvent callback
  _ <- FS.watchTree mgr templateDir filterEvent callback
  _ <- FS.watchTree mgr copyDir filterEvent callback
  forever $ threadDelay 1_000_000
  where
    sourceDir = projectSourceDir project
    assetsDir = projectAssetsDir project
    templateDir = projectTemplateDir project
    copyDir = projectCopyDir project

    filterEvent :: FS.Event -> Bool
    filterEvent (FS.Modified {}) = True
    filterEvent (FS.Added {}) = True
    filterEvent (FS.Removed {}) = True
    filterEvent _ = False

    callback event = do
      result <- runExceptT $ buildProjectWith processors project
      let path = normalise $ FS.eventPath event
          relPath = makeRelative (projectRoot project) path
          time = T.pack $ show $ FS.eventTime event
          isInBuildDir = projectOutDir project `isPrefixOf` path
      unless isInBuildDir $ do
        case result of
          Left errorMessage -> printErrorMessage (T.pack errorMessage)
          Right _ -> printWatchMessage time (T.pack relPath)
