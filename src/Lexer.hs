module Lexer(Lexer.lexer) where

import Lexer.Token
import Data.Char
import Control.Monad
import Data.Maybe
import Data.List

lexer :: String -> [Token]
lexer "" = []
lexer src =
  case token of
    Unknown -> [Unknown]
    _       -> token:lexer newsrc
  where 
    (token, newsrc) = nextToken . dropWhile isSpace $ src

nextToken :: String -> (Token, String)
nextToken src = fromMaybe (Unknown, src) token
  where 
    token = readWord src `mplus`
        readSymbol src

readWord :: String -> Maybe (Token, String)
readWord src = 
  if not . null $ word
    then Just (token, newsrc)
    else Nothing

  where
    (word, newsrc) = span isword src

    isword c = isAlphaNum c || c == '_'

    keyword = tokenizeKeyword word

    token = case keyword of
      Just k -> Keyword k
      Nothing -> Identifier word

readSymbol :: String -> Maybe (Token, String)
readSymbol src = do
  (symbol, token) <- tokenize
  let newsrc = drop (length symbol) src
  return (token, newsrc)

  where
    tokenize :: Maybe (String, Token)
    tokenize = find (\(s,_) -> s `isPrefixOf` src) symbols

tokenizeNumber :: String -> Maybe Token
tokenizeNumber = undefined
