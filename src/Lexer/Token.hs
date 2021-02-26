module Lexer.Token where

data Keyword =
    Return
  | Int
  deriving (Show)

data Token =
    Keyword Keyword
  | LParenthesis
  | RParenthesis
  | LCurlyBrace
  | RCurlyBrace
  | Semicolon
  | IntegerLiteral Int
  | Identifier String
  | Unknown
  deriving (Show)

tokenizeKeyword :: [Char] -> Maybe Keyword
tokenizeKeyword "int"    = Just Int
tokenizeKeyword "return" = Just Return
tokenizeKeyword _        = Nothing
