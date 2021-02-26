module Lexer(Lexer.lexer) where

import Lexer.Token
import Data.Char
import Control.Monad
import Data.Maybe

lexer :: String -> [Token]
lexer "" = []
lexer src =
  case token of
    Unknown -> [Unknown]
    _       -> token:lexer newsrc
  where 
    (token, newsrc) = nextToken . dropWhile isSpace $ src

nextToken :: String -> (Token, String)
nextToken src = fromMaybe (Unknown, src) (readWord src)

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

tokenizeWord :: String -> Maybe Token
tokenizeWord str = Just $ Identifier str

tokenizeSymbol :: Char -> Maybe Token
tokenizeSymbol '{' = Just LCurlyBrace
tokenizeSymbol '}' = Just RCurlyBrace
tokenizeSymbol '(' = Just LParenthesis
tokenizeSymbol ')' = Just RParenthesis
tokenizeSymbol ';' = Just RParenthesis

tokenizeNumber :: String -> Maybe Token
tokenizeNumber = undefined
