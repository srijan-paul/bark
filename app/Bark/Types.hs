module Bark.Types
  ( Project (..),
    Post (..),
    HTMLPage (..),
    PostFrontMatter (..),
    Preprocessor,
    Postprocessor,
    ErrorMessage,
  )
where

import Bark.FrontMatter (PostFrontMatter (..))
import Control.Monad.Except (ExceptT)
import qualified Data.Text as T
import qualified Text.Mustache.Types as Mustache

type ErrorMessage = String

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

data HTMLPage = HTMLPage
  { htmlPagePost :: Post,
    htmlPageContent :: T.Text
  }

type Preprocessor = Project -> Post -> ExceptT ErrorMessage IO Post

type Postprocessor = Project -> HTMLPage -> ExceptT ErrorMessage IO HTMLPage

data Processor
  = OnPost Preprocessor
  | OnHTML Postprocessor
