module Bark.FrontMatter (tokenize, Token (..), parse, parseFromTokens) where

import Data.Char (isAlphaNum, isSpace)
import Data.HashMap.Strict (HashMap, empty, insert)
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

parseList :: [Value] -> [Token] -> ([Value], [Token])
parseList acc tokens =
  case tokens of
    TRSqBrac : toks -> (reverse acc, toks)
    tok : toks ->
      let (value, restTokens) = parse' tokens
       in parseList (value : acc) restTokens
    [] -> error "Expected ']' to close list, but found end of input"

type MetaMapEntry = (Text, Value)

type MetaMap = HashMap Text Value

parseMapEntry :: [Token] -> (MetaMapEntry, [Token])
parseMapEntry [TKey key] = error "Expected ':' after map key."
parseMapEntry (TKey key : [TColon]) = error "Unexpected end of input while parsing map entry."
parseMapEntry (TKey key : TColon : toks) =
  let (value, restOfTokens) = parse' toks
   in ((pack key, value), restOfTokens)
parseMapEntry _ = error "Bad call to `parseMapEntry`. Please file a bug report"

parseMap :: MetaMap -> [Token] -> (MetaMap, [Token])
parseMap acc [] = (acc, [])
parseMap acc tokstream@(token : toks) =
  case token of
    TKey key ->
      let ((k, v), restOfToks) = parseMapEntry tokstream
       in parseMap (insert k v acc) restOfToks
    TRBrac -> (acc, toks)
    _ -> undefined

parse' :: [Token] -> (Value, [Token])
parse' tokstream@(token : rest) =
  case token of
    TString str -> (String $ T.pack str, rest)
    TLSqBrac ->
      let (values, remainingTokens) = parseList [] rest
       in (Array $ Vec.fromList values, remainingTokens)
    TLBrac ->
      let (hmap, remainingTokens) = parseMap empty rest
       in (Object hmap, remainingTokens)
    TError errMessage -> error errMessage
    unknownToken -> error $ "Unexpected token " ++ show unknownToken
parse' [] = undefined

parseFromTokens :: [Token] -> Value
parseFromTokens = fst . parse'

parse :: String -> Value
parse = parseFromTokens . tokenize
