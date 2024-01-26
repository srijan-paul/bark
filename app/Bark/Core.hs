{-# LANGUAGE OverloadedStrings #-}

module Bark.Core (getPostFromMdfile, Project (..), postToHtml, buildPost) where

import Bark.FrontMatter (PostFrontMatter (..), parseFrontMatter)
import Bark.Internal.IOUtil (ErrorMessage, tryReadFileBS, tryReadFileT)
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Arrow (ArrowChoice (left))
import Control.Monad.Except (ExceptT, MonadIO (liftIO), liftEither)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.List (findIndex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, makeRelative, replaceExtension, takeBaseName, takeDirectory, (</>))
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

-- | Represents a markdown file with metadata,
-- that will later be rendered as HTML.
data Post = Post
  { postPath :: FilePath,
    postDstPath :: FilePath,
    postFrontMatter :: PostFrontMatter,
    postContent :: T.Text,
    postUrl :: FilePath
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
        postUrl = url
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
          HM.fromList
            [ ("content", Mustache.String postHtml),
              ("metadata", metadata)
            ]
      output = Mustache.substitute template postData
      outPath = postDstPath post
  liftIO $ createDirectoryIfMissing True $ takeDirectory outPath
  liftIO $ TIO.writeFile outPath output