{-# LANGUAGE TupleSections #-}

module Bark.FrontMatter (tokenize, Token (..), parse, parseFromTokens) where

import qualified Data.Bifunctor as Bifunctor
import Data.Char (isAlphaNum, isSpace)
import Data.HashMap.Strict (HashMap, empty, insert)
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

parseList :: [Value] -> [Token] -> Either ParseError ([Value], [Token])
parseList acc tokens =
  case tokens of
    TRSqBrac : toks -> Right (reverse acc, toks)
    tok : toks ->
      case parse' tokens of
        Right (value, restOfTokens) -> parseList (value : acc) restOfTokens
        Left errMsg -> Left errMsg
    [] -> Left "Expected ']' to close list, but found end of input"

type MetaMapEntry = (Text, Value)

type MetaMap = HashMap Text Value

parseMapEntry :: [Token] -> Either ParseError (MetaMapEntry, [Token])
parseMapEntry [TKey key] =
  Left "Expected ':' after map key."
parseMapEntry (TKey key : [TColon]) =
  Left "Unexpected end of input while reading map entry."
parseMapEntry (TKey key : TColon : toks) =
  Bifunctor.first (T.pack key,) <$> parse' toks
parseMapEntry token =
  Left $ "Unexpected token while parsing map entry: " ++ show token

parseMap :: MetaMap -> [Token] -> Either ParseError (MetaMap, [Token])
parseMap acc [] = Right (acc, [])
parseMap acc tokstream@(token : toks) =
  case token of
    TKey key ->
      case parseMapEntry tokstream of
        Right ((k, v), restOfToks) -> parseMap (insert k v acc) restOfToks
        Left err -> Left err
    TRBrac -> Right (acc, toks)
    other -> Left $ "Unexpected token while reading map: " ++ show other

parse' :: [Token] -> Either ParseError (Value, [Token])
parse' tokstream@(token : rest) =
  case token of
    TString str -> Right (String $ T.pack str, rest)
    TLSqBrac ->
      Bifunctor.first (Array . Vec.fromList) <$> parseList [] rest
    TLBrac ->
      Bifunctor.first Object <$> parseMap empty rest
    TError errMessage -> Left errMessage
    unknownToken -> Left $ "Unexpected token " ++ show unknownToken
parse' [] = Right (Null, [])

parseFromTokens :: [Token] -> Either ParseError Value
parseFromTokens tokens = fst <$> parse' tokens

parse :: String -> Either ParseError Value
parse = parseFromTokens . tokenize
