{-# LANGUAGE OverloadedStrings #-}

module Bark.Preprocess (TransFormer, applyTransformers, highLightSnippets) where

import qualified Data.List as L
import Data.Map ((!))
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Skylighting
import qualified Skylighting as Fmt
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.HTML.TagSoup as H

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

type TransFormer = [H.Tag T.Text] -> [H.Tag T.Text]

highLightSnippets :: TransFormer
highLightSnippets = go
  where
    go :: [H.Tag T.Text] -> [H.Tag T.Text]
    go (open@(H.TagOpen "code" attrs) : (H.TagText txt) : rest) =
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
