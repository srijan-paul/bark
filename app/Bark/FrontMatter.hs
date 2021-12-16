module Bark.FrontMatter (tokenize, Token (..), MetaData) where

import Data.Char (isAlphaNum, isSpace)
import Data.HashMap.Strict (HashMap, empty)
import Data.List (takeWhile)
import Data.Text (Text)
import Debug.Trace (trace)

data MetaData = HashMap String Text

data Token
  = TString String
  | TKey String
  | TColon
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
  | c == ':' = TColon : tokenize rest
  | c == '"' =
    let (stringValue, restOfText) = span (/= '"') rest
     in if restOfText /= ""
          then TString stringValue : tokenize (tail restOfText)
          else [TError "Unterminated string"]
  | isAlphaNum c =
    let (ident, restOfText) = span isAlphaNum text
     in TKey ident : tokenize restOfText
  | otherwise = [TError $ "Unexpected character: " ++ [c]]
