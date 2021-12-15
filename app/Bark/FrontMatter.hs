module Bark.FrontMatter (tokenize, Token (..)) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (takeWhile)
import Data.Map (Map, empty)
import Data.Text (Text)
import Debug.Trace (trace)

data Token
  = TString String
  | TKey String
  | TLBrac
  | TRBrac
  | TError String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize "" = []
tokenize text@(c : rest)
  | isSpace c = tokenize rest
  | c == '[' = TLBrac : tokenize rest
  | c == ']' = TRBrac : tokenize rest
  | c == '"' =
    let (stringValue, restOfText) = span (/= '"') rest
     in if restOfText /= ""
          then TString stringValue : tokenize (tail restOfText)
          else [TError "Unterminated string"]
  | otherwise =
    let (ident, restOfText) = span isAlphaNum text
     in TKey ident : tokenize restOfText
