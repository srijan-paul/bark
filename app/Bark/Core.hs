{-# LANGUAGE OverloadedStrings #-}

module Bark.Core
  ( getPostFromMdfile,
    Project (..),
    Post (..),
    postToHtml,
    buildPost,
    buildProject,
  )
where

import Bark.FrontMatter (PostFrontMatter (..), parseFrontMatter)
import Bark.Internal.IOUtil (ErrorMessage, copyDirectory, tryReadFileBS, tryReadFileT)
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Arrow (ArrowChoice (left))
import Control.Monad.Except (ExceptT, MonadIO (liftIO), liftEither, when)
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

-- | Represents a Bark project.
--  Contains absolute paths to various directories.
data Project = Project
  { projectRoot :: FilePath,
    projectSourceDir :: FilePath,
    projectOutDir :: FilePath,
    projectAssetsDir :: FilePath,
    projectTemplateDir :: FilePath
  }
  deriving (Show)

-- | Represents a markdown file with metadata,
-- that will later be rendered as HTML.
data Post = Post
  { -- | Absolute path to the markdown file that contains the source for this post.
    postPath :: FilePath,
    -- | Absolute path where the post will be rendered.
    postDstPath :: FilePath,
    -- | The YAML frontmatter found at the top of every markdown page.
    -- There is one mandatory field called "template", and the rest is upto the user.
    postFrontMatter :: PostFrontMatter,
    -- | The markdown content of the post, frontmatter included.
    postContent :: T.Text,
    -- | The URL where the post will be hosted, relative to the root.
    --  E.g: "my-website/src/blog/hello-world.md" -> "blog/hello-world/index.html"
    postUrl :: FilePath,
    -- | Any additional fields needed by the post's template.
    -- During building, the post has three data fields that it can access:
    -- - **content**: the markdown content converted to HTML.
    -- - **meta**: the metadata from the frontmatter.
    -- - **posts**: an array of all posts in the project.
    --
    -- In addition to these, posts can be modified to have their own data fields.
    -- For example, a **website_url** field that stores the URL where the site containing all pages is hosted.
    postOtherData :: [(T.Text, Mustache.Value)]
  }
  deriving (Show)

urlFromMdPath :: Project -> FilePath -> FilePath
urlFromMdPath Project {projectSourceDir = sourceDir} mdFilePath
  | takeBaseName mdFilePath == "index" = replaceExtension (makeRelative sourceDir mdFilePath) ".html"
  | otherwise = makeRelative sourceDir (dropExtension mdFilePath </> "index.html")

-- | Parse a post from a markdown file.
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

buildPost :: Project -> Post -> ExceptT ErrorMessage IO ()
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
      outPath = postDstPath post
  liftIO $ createDirectoryIfMissing True $ takeDirectory outPath
  liftIO $ TIO.writeFile outPath output

-- | Get a list of all posts in the project.
getPosts :: Project -> ExceptT ErrorMessage IO [Post]
getPosts project = do
  files <- liftIO $ getFilesRecursive (projectSourceDir project)
  liftIO $ createDirectoryIfMissing True (projectOutDir project)
  let markdownFiles = filter ((`elem` [".markdown", ".md"]) . takeExtension) files
  mapM (getPostFromMdfile project) markdownFiles

-- | Build a bark project
buildProject :: Project -> ExceptT ErrorMessage IO ()
buildProject project = do
  posts' <- getPosts project
  -- Every post has an additional data field called `posts`.
  -- It is an array of all posts in the project.
  let allPostsMeta =
        Mustache.Array $
          Vector.fromList $
            map (Mustache.Object . fmMetaData . postFrontMatter) posts'
      posts = map (\p -> p {postOtherData = [("posts", allPostsMeta)]}) posts'

  mapM_ (buildPost project) posts
  liftIO $ do
    hasAssets <- doesDirectoryExist assetsDir
    when hasAssets $
      copyDirectory assetsDir (outDir </> makeRelative rootDir assetsDir)
  where
    rootDir = projectRoot project
    src = projectSourceDir project
    outDir = projectOutDir project
    assetsDir = projectAssetsDir project
