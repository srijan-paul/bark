{-# LANGUAGE OverloadedStrings #-}

module Bark.Processors.SyntaxHighlight (highlightSnippets) where

import Bark.Types (HTMLPage (..), Postprocessor)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Skylighting (defaultFormatOpts, defaultSyntaxMap, tokenize)
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

-- | Go over the attributes of a <code> tag, and find the syntax map key for language used.
--  Returns "default" if none found.
getSyntaxKeyFromAttrs :: [H.Attribute T.Text] -> T.Text
getSyntaxKeyFromAttrs attrs =
  let languageAttr = snd <$> List.find (T.isPrefixOf "language-" . snd) attrs
      syntaxMapKey = M.fromMaybe "default" (languageAttr >>= T.stripPrefix "language-")
   in syntaxMapKey

-- | Highlight [code] where the attributes are given by the [attrs] list.
highlightBlock :: [H.Attribute T.Text] -> T.Text -> Either String [H.Tag T.Text]
highlightBlock attrs code =
  let syntaxKey = getSyntaxKeyFromAttrs attrs
      tokens = getTokensFromText syntaxKey code
      html = Fmt.formatHtmlBlock defaultFormatOpts <$> tokens
      renderedHtml = renderHtml <$> html
      parsedHtml = H.parseTags . TL.toStrict <$> renderedHtml
   in parsedHtml

-- | Highlight all code present inside code tags that are nested in pre tags.
-- | This will highlight all occurrences of the pattern <pre> <code> "foo" </code></pre>
highLightAST :: [H.Tag T.Text] -> [H.Tag T.Text]
highLightAST = go
  where
    go :: [H.Tag T.Text] -> [H.Tag T.Text]
    -- <pre> <code> txt ...
    go (H.TagOpen "pre" _ : open@(H.TagOpen "code" attrs) : (H.TagText txt) : rest) =
      case highlightBlock attrs txt of
        Left foo -> error foo
        Right tags -> (open : tags) ++ go rest
    go (x : other) = x : go other
    go [] = []

highlightSnippets' :: T.Text -> T.Text
highlightSnippets' = H.renderTags . highLightAST . H.parseTags

-- | Syntax highlight all <code> snippets in the AST.
highlightSnippets :: Postprocessor
highlightSnippets _ page@(HTMLPage _ html) = do
  let highlightedHTML = highlightSnippets' html
   in return $ page {htmlPageContent = highlightedHTML}
