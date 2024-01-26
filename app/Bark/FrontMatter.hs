{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bark.FrontMatter (parseFrontMatter, PostFrontMatter (..), toMustacheValue) where

import Bark.Internal.IOUtil (ErrorMessage)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (typeMismatch, (.:))
import qualified Data.ByteString as BS
import Data.Frontmatter
  ( IResult (Done, Fail, Partial),
    parseYamlFrontmatter,
  )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Yaml (FromJSON (..), Value (..))
import qualified Data.Yaml.Aeson as Aeson
import qualified Text.Mustache.Types as Mustache

data PostFrontMatter = PostFrontMatter
  { fmTemplate :: T.Text, -- template to use for rendering.
    fmMetaData :: Mustache.Value -- other metadata for the post (will also include 'template' field)
  }
  deriving (Show)

instance FromJSON PostFrontMatter where
  parseJSON :: Value -> Aeson.Parser PostFrontMatter
  parseJSON (Aeson.Object o) = do
    template <- o .: "template"
    return $
      PostFrontMatter
        { fmTemplate = template,
          fmMetaData = toMustacheValue $ Object o
        }

  -- frontmatter must be an object. Anything else results in parse failure.
  parseJSON other = typeMismatch "Object" other

parseFrontMatter :: FilePath -> BS.ByteString -> Either ErrorMessage PostFrontMatter
parseFrontMatter filePath contents = do
  case parseYamlFrontmatter contents of
    Done _ fm -> Right fm
    Fail _ _ errMsg -> Left $ "Failed to parse YAML frontmatter for " ++ filePath ++ ": " ++ errMsg
    Partial _ -> Left $ "Unexpected end of input when trying to parse frontmatter for " ++ filePath

toMustacheValue :: Value -> Mustache.Value
toMustacheValue (Array a) = Mustache.Array $ fmap toMustacheValue a
toMustacheValue (String text) = Mustache.String text
toMustacheValue (Number n) = Mustache.Number n
toMustacheValue (Bool b) = Mustache.Bool b
toMustacheValue (Object o) =
  -- TODO: can I not use `show` here? Any better way to convert the key to Text?
  let toMustachePair (k, v) = (T.pack (show k), toMustacheValue v)
   in Mustache.Object $ HM.fromList $ map toMustachePair (toList o)
