module Lexer.Token where

import Data.Maybe
import Data.List

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
  | DoubleLiteral Double
  | Identifier String
  | Unknown
  deriving (Show)

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
    ( "++", undefined    ),
    ( "->", undefined    ),
    ( "*",  undefined    ),
    ( "+",  undefined    ),
    ( "-",  undefined    ),
    ( "/",  undefined    )]
