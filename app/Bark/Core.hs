{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bark.Core (parseMetaDataFromFile) where

import Bark.Internal.IOUtil (ErrorMessage, tryReadFileBS)
import Control.Monad.Except (ExceptT, liftEither)
import Data.Aeson.Types (typeMismatch, (.:))
import qualified Data.ByteString as BS
import Data.Frontmatter
import qualified Data.Text as T
import Data.Yaml (FromJSON (..), Object, Value)
import qualified Data.Yaml.Aeson as Aeson

data PostFrontMatter = PostFrontMatter
  { fmTemplate :: T.Text, -- template to use for rendering.
    fmMetaData :: Object -- other metadata for the post (will also include 'template')
  }
  deriving (Show)

instance FromJSON PostFrontMatter where
  parseJSON :: Value -> Aeson.Parser PostFrontMatter
  parseJSON (Aeson.Object o) = do
    template <- o .: "template"
    return $ PostFrontMatter {fmTemplate = template, fmMetaData = o}

  -- frontmatter must be an object. Anything else results in parse failure.
  parseJSON other = typeMismatch "Object" other

parseMetaDataFromFile :: FilePath -> BS.ByteString -> Either ErrorMessage PostFrontMatter
parseMetaDataFromFile filePath contents = do
  case parseYamlFrontmatter contents of
    Done _ fm -> Right fm
    Fail _ _ errMsg -> Left $ "Failed to parse YAML frontmatter for " ++ filePath ++ ":" ++ errMsg
    Partial _ -> Left $ "Unexpected end of input when trying to parse frontmatter for " ++ filePath

data Post = Post
  { postPath :: FilePath,
    postRelativePath :: T.Text,
    postDstPath :: T.Text,
    postFrontMatter :: PostFrontMatter,
    postContent :: T.Text,
    postUrl :: T.Text
  }
  deriving (Show)

-- | Parse a post from a markdown file.
getPostFromMdfile :: FilePath -> FilePath -> ExceptT ErrorMessage IO Post
getPostFromMdfile projectRoot filePath = do
  fileContents <- tryReadFileBS filePath
  metadata <- liftEither $ parseMetaDataFromFile filePath fileContents
  undefined
