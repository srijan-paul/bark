module Bark.Core (getPostFromMdfile, Project (..), postToHtml) where

import Bark.FrontMatter (PostFrontMatter, parseFrontMatter)
import Bark.Internal.IOUtil (ErrorMessage, tryReadFileBS)
import Commonmark (Html, ParseError, commonmarkWith, defaultSyntaxSpec, renderHtml)
import Commonmark.Extensions (gfmExtensions)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import System.FilePath (makeRelative, (</>))

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
-- that should be rendered as HTML.
data Post = Post
  { postPath :: FilePath,
    postDstPath :: FilePath,
    postFrontMatter :: PostFrontMatter,
    postContent :: T.Text,
    postUrl :: FilePath
  }
  deriving (Show)

-- | Parse a post from a markdown file.
getPostFromMdfile :: Project -> FilePath -> ExceptT ErrorMessage IO Post
getPostFromMdfile project filePath = do
  content <- tryReadFileBS filePath
  frontmatter <- liftEither $ parseFrontMatter filePath content
  let url = makeRelative (projectRoot project) filePath
      dstPath = projectOutDir project </> url
  return $
    Post
      { postPath = filePath,
        postDstPath = dstPath,
        postFrontMatter = frontmatter,
        postContent = T.decodeUtf8 content,
        postUrl = url
      }

md2Html :: FilePath -> T.Text -> Either ErrorMessage TL.Text
md2Html path contents =
  bimap show renderHtml (runIdentity parseResult)
  where
    parseResult =
      commonmarkWith
        (defaultSyntaxSpec <> gfmExtensions)
        path
        contents ::
        (Identity (Either ParseError (Html ())))

stripFrontMatter :: T.Text -> T.Text
stripFrontMatter markdown = undefined

postToHtml :: Post -> Either ErrorMessage TL.Text
postToHtml Post {postPath = path, postContent = content} =
  md2Html
    path
    (stripFrontMatter content)