module Bark.FrontMatter where

import Data.Char (isAlphaNum)
import Data.List (takeWhile)
import Data.Map (Map, empty)
import Data.Text (Text)

data Token
  = TString String
  | TKey String
  | TLBrac
  | TRBrac
  | TError String

tokenize :: String -> [Token]
tokenize "" = []
tokenize text@(c : rest) =
  case c of
    '"' ->
      let restOfText = dropWhile (/= '"') rest
          stringValue = takeWhile (/= '"') rest
       in if restOfText == "" -- unterminated string
            then TString stringValue : tokenize (tail restOfText)
            else [TError "Unterminated string"]
    '[' -> TLBrac : tokenize rest
    ']' -> TRBrac : tokenize rest
    _ ->
      let ident = takeWhile isAlphaNum text
       in TKey ident : tokenize (dropWhile isAlphaNum text)
