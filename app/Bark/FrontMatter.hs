module Bark.FrontMatter (tokenize, Token (..), parse') where

import Data.Char (isAlphaNum, isSpace)
import Data.HashMap.Strict (HashMap, empty)
import Data.List (takeWhile)
import qualified Data.List as List
import Data.Text as T (Text, pack)
import qualified Data.Vector as Vec (fromList)
import Text.Mustache.Types (Value (..))

data Token
  = TString String -- ".*"
  | TKey String -- [a-zA-Z0-9_]+
  | TColon -- :
  | TLSqBrac -- [
  | TRSqBrac -- ]
  | TLBrac -- {
  | TRBrac -- }
  | TError String
  deriving (Eq, Show)

type ParseError = String

tokenize :: String -> [Token]
tokenize "" = []
tokenize text@(c : rest)
  | isSpace c = tokenize rest
  | c == '[' = TLSqBrac : tokenize rest
  | c == ']' = TRSqBrac : tokenize rest
  | c == ':' = TColon : tokenize rest
  | c == '{' = TLBrac : tokenize rest
  | c == '}' = TRBrac : tokenize rest
  | c == '"' =
    let (stringValue, restOfText) = span (/= '"') rest
     in if restOfText /= ""
          then TString stringValue : tokenize (tail restOfText)
          else [TError "Unterminated string"]
  | isAlphaNum c =
    let (ident, restOfText) = span isAlphaNum text
     in TKey ident : tokenize restOfText
  | otherwise = [TError $ "Unexpected character: " ++ [c]]

parseList :: [Value] -> [Token] -> (Value, [Token])
parseList _ [] = undefined
parseList acc (tok : toks) =
  let (value, restTokens) = parse' toks
   in if null restTokens then 
        error ""
      else if head restTokens == TRSqBrac then
        let list = Array $ Vec.fromList $ reverse (value : acc)
          in (list, tail restTokens)
      else 
        parseList (value : acc) restTokens

parse' :: [Token] -> (Value, [Token])
parse' tokstream@(token : rest) =
  case token of
    TString str -> (String $ T.pack str, rest)
    TLBrac -> parseList [] rest
    _ -> error "Unexpected token"
parse' [] = undefined
