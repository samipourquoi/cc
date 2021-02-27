module Lexer.Token where

import Data.Maybe
import Data.List

data Literal =
    IntegerLiteral Int
  | DoubleLiteral Double
  deriving (Show, Eq)

data Keyword =
    Return
  | Int
  deriving (Show, Eq)

data Token =
    Keyword Keyword
  | Literal Literal
  | LParenthesis
  | RParenthesis
  | LCurlyBrace
  | RCurlyBrace
  | Semicolon
  | Identifier String
  | Comma
  | Unknown
  deriving (Show, Eq)

tokenizeKeyword :: [Char] -> Maybe Keyword
tokenizeKeyword "int"    = Just Int
tokenizeKeyword "return" = Just Return
tokenizeKeyword _        = Nothing

symbols =
   [( "{",  LCurlyBrace  ),
    ( "}",  RCurlyBrace  ),
    ( "(",  LParenthesis ),
    ( ")",  RParenthesis ),
    ( ";",  Semicolon    ),
    ( ",",  Comma        ),
    ( "++", undefined    ),
    ( "->", undefined    ),
    ( "*",  undefined    ),
    ( "+",  undefined    ),
    ( "-",  undefined    ),
    ( "/",  undefined    )]
