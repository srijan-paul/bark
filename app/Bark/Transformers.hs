{-# LANGUAGE OverloadedStrings #-}

module Bark.Transformers (TransFormer, applyTransformers, highLightSnippets) where

import qualified Data.List as L
import Data.Map ((!))
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Skylighting
import qualified Skylighting as Fmt
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.HTML.TagSoup as H

-- | Tokenize [text], assuming a regex grammar for a language [langName]
getTokensFromText :: T.Text -> T.Text -> Either String [Fmt.SourceLine]
getTokensFromText langName text =
  let syntax =
        M.fromMaybe
          (defaultSyntaxMap ! T.pack "Default")
          (Fmt.lookupSyntax langName defaultSyntaxMap)
      tokenizerConfig =
        Fmt.TokenizerConfig
          { Fmt.syntaxMap = defaultSyntaxMap,
            Fmt.traceOutput = False
          }
   in tokenize tokenizerConfig syntax text

-- | Highlight [code] where the attributes are given by the [attrs] list.
highlightBlock :: [H.Attribute T.Text] -> T.Text -> Either String [H.Tag T.Text]
highlightBlock attrs code =
  let langAttr = L.find (T.isPrefixOf "language-" . snd) attrs
      language = maybe "language-default" snd langAttr
      syntaxMapKey = M.fromJust $ T.stripPrefix "language-" language
      tokens = getTokensFromText syntaxMapKey code
      html = Fmt.formatHtmlBlock defaultFormatOpts <$> tokens
      renderedHtml = renderHtml <$> html
      parsedHtml = H.parseTags . TL.toStrict <$> renderedHtml
   in parsedHtml

-- | A Transformer takes an HTML AST (A list of tags) and returns a modified
-- AST.
type TransFormer = [H.Tag T.Text] -> [H.Tag T.Text]

-- | Highlight all code present inside code tags that are nested in pre tags. 
-- | This will highlight all occurrences of the pattern <pre> <code> "foo" </code></pre>
highLightSnippets :: TransFormer
highLightSnippets = go
  where
    go :: [H.Tag T.Text] -> [H.Tag T.Text]
    -- <pre> <code> txt ...
    go (H.TagOpen "pre" _ : open@(H.TagOpen "code" attrs) : (H.TagText txt) : rest) =
      case highlightBlock attrs txt of
        Left foo -> error foo
        Right tags -> (open : tags) ++ go rest
    go (x : other) = x : go other
    go [] = []

applyTransformers :: [TransFormer] -> T.Text -> T.Text
applyTransformers [] f = f
applyTransformers funcs html =
  let ast = H.parseTags html
      finalAst = go funcs ast
   in H.renderTags finalAst
  where
    go :: [TransFormer] -> [H.Tag T.Text] -> [H.Tag T.Text]
    go [] tags = tags
    go (f : fs) tags = go fs (f tags)
