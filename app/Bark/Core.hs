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
import Control.Monad (forever)
import Control.Monad.Except (ExceptT, MonadIO (liftIO), foldM, liftEither, runExceptT, when)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.List (findIndex)
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
    replaceExtension,
    takeBaseName,
    takeDirectory,
    takeExtension,
    (</>),
  )
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

buildProjectImpl :: Project -> [Postprocessor] -> [Post] -> ExceptT ErrorMessage IO ()
buildProjectImpl project postprocessors posts = do
  -- Convert markdown posts to HTML pages.
  pages <- mapM (buildPost project) posts
  -- apply any post compilation processors (e.g. syntax highlighting, etc.)
  processedPages <- mapM applyHtmlProcessors pages
  -- write the processed pages to disk
  mapM_ writeHtmlPage processedPages
  -- copy assets directory to the output directory
  liftIO $ do
    hasAssets <- doesDirectoryExist assetsDir
    when hasAssets $
      copyDirectory assetsDir (outDir </> makeRelative rootDir assetsDir)
  where
    rootDir = projectRoot project
    outDir = projectOutDir project
    assetsDir = projectAssetsDir project

    applyHtmlProcessors :: HTMLPage -> ExceptT ErrorMessage IO HTMLPage
    applyHtmlProcessors post = foldM (\post p -> p project post) post postprocessors

addPostListToMeta :: [Post] -> ExceptT ErrorMessage IO [Post]
addPostListToMeta posts' = do
  -- Every post has an additional data field called `posts`.
  -- It is an array of all posts in the project.
  let allPosts =
        (Mustache.Array . Vector.fromList) $
          map (Mustache.Object . fmMetaData . postFrontMatter) posts'
      posts = map (\p -> p {postOtherData = [("posts", allPosts)]}) posts'
  return posts

-- | Build a bark project
buildProject :: Project -> ExceptT ErrorMessage IO ()
buildProject project = do
  posts <- getPosts project >>= addPostListToMeta
  buildProjectImpl project [] posts

buildProjectWith :: [Postprocessor] -> Project -> ExceptT ErrorMessage IO ()
buildProjectWith postprocessors project = do
  posts <- getPosts project >>= addPostListToMeta
  buildProjectImpl project postprocessors posts

watchProjectWith :: [Postprocessor] -> Project -> IO FS.StopListening
watchProjectWith processors project = FS.withManager $ \mgr -> do
  _ <- FS.watchTree mgr rootDir (const True) (const callback)
  forever (threadDelay 10_000_00)
  where
    rootDir = projectRoot project
    callback = do
      result <- runExceptT $ buildProjectWith processors project
      case result of
        Left errorMessage -> print errorMessage
        Right _ -> return ()
