{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bark.Types
  ( Project (..),
    ProjectConfig (..),
    Post (..),
    HTMLPage (..),
    PostFrontMatter (..),
    Processor,
    ErrorMessage,
    AssetFile (..),
    Plugin (..),
    fromList,
    toList,
    Compilation (..),
    newCompilation,
    processorOfPlugin,
    htmlFromPost,
    module Mustache,
    module Control.Monad.State,
    module Control.Monad.Except,
  )
where

import Bark.FrontMatter (PostFrontMatter (..))
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson.Types (typeMismatch, (.:))
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yml
import Text.Mustache.Types as Mustache

type ErrorMessage = String

--- | Converts a list of values to an array.
fromList :: [Mustache.Value] -> Mustache.Value
fromList = Mustache.Array . Vector.fromList

-- | Converts a Mustache value to a list.
toList :: Mustache.Array -> [Mustache.Value]
toList = Vector.toList

-- | Represents a Bark project.
--  Contains absolute paths to various directories.
data Project = Project
  { projectRoot :: FilePath,
    projectSourceDir :: FilePath,
    projectOutDir :: FilePath,
    projectAssetsDir :: FilePath,
    projectTemplateDir :: FilePath,
    projectCopyDir :: FilePath
  }
  deriving (Show, Eq)

data ProjectConfig = ProjectConfig
  { configSourceDir :: FilePath,
    configOutDir :: FilePath,
    configAssetsDir :: FilePath,
    configTemplateDir :: FilePath,
    configCopyDir :: FilePath
  }
  deriving (Show, Eq)

instance Yml.FromJSON ProjectConfig where
  parseJSON :: Yml.Value -> Yml.Parser ProjectConfig
  parseJSON (Yml.Object o) = do
    sourceDir <- o .: "source"
    outDir <- o .: "out"
    assetsDir <- o .: "assets"
    templateDir <- o .: "template"
    copyDir <- o .: "copy"
    return $
      ProjectConfig
        { configSourceDir = sourceDir,
          configOutDir = outDir,
          configAssetsDir = assetsDir,
          configTemplateDir = templateDir,
          configCopyDir = copyDir
        }
  parseJSON other = typeMismatch "Object" other

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
    -- - **posts**: an array of all posts in the project, after they've been preprocessed.
    --
    -- In addition to these, posts can be modified to have their own data fields.
    -- For example, a **website_url** field that stores the URL where the site containing all pages is hosted.
    postData :: Mustache.Object
  }
  deriving (Show)

-- | Represents a file in the project's assets directory (images, media, CSS, etc.)
data AssetFile = AssetFile
  { assetFilePath :: FilePath,
    assetDstPath :: FilePath,
    assetProject :: Project,
    assetContent :: Maybe T.Text
  }

-- | An compiled HTML page that will be written to disk.
data HTMLPage = HTMLPage
  { -- | The markdown Post from which this HTML page was generated.
    htmlPagePost :: Maybe Post,
    -- | The HTML content of the page.
    htmlPageContent :: T.Text,
    -- | The (absolute) output path of this HTML page.
    htmlPagePath :: FilePath
  } deriving (Show)

htmlFromPost :: Post -> T.Text -> HTMLPage
htmlFromPost p content = HTMLPage (Just p) content (postDstPath p)

data Compilation = Compilation
  { compilationProject :: Project,
    compilationPosts :: [Post],
    compilationPages :: [HTMLPage]
  }

newCompilation :: Project -> [Post] -> [HTMLPage] -> Compilation
newCompilation = Compilation

type Processor a = StateT Compilation (ExceptT ErrorMessage IO) a

data Plugin
  = BeforeBuild (Processor ())
  | AfterBuild (Processor ())

processorOfPlugin :: Plugin -> Processor ()
processorOfPlugin (BeforeBuild p) = p
processorOfPlugin (AfterBuild p) = p
