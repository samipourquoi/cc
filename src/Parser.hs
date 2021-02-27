{-# LANGUAGE LambdaCase #-}

module Parser(parser, AST) where

import Lexer.Token
import Control.Applicative
import Control.Monad


-- | Entry point of the parsing stage.
parser :: [Token] -> AST
parser = undefined


newtype Parser a = 
  Parser ([Token] -> [(a, [Token])])

-- | A parser can be seen as a monad: it's a series of transformation.
-- This was heavily based on this amazing paper: 
-- > https://pdfs.semanticscholar.org/d999/08c1e0bcda1469835e199d0d7ef4d71f4a2c.pdf
instance Monad Parser where
  return a = Parser (\tokens -> [(a, tokens)])
  p >>= f  = Parser (\tokens -> concat 
    [parse (f a) tokens' 
    | (a, tokens') <- parse p tokens])
instance MonadPlus Parser where
  mzero      = Parser (const [])
  mplus p p' = Parser (\tokens -> parse p tokens ++ parse p' tokens)

-- | This instance definitions aren't really necessary
-- but we need them to be able to instanciate Monad
-- and MonadPlus on Parser.
instance Functor Parser where
  fmap = liftM
instance Applicative Parser where
  pure = return
  (<*>) = ap
instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

-- | Just for debug sake.
instance Show (Parser a) where 
  show _ = "Parser"

type AST = [AST_FUNCTION]

-- | Applies a parser to a list of tokens.
parse :: Parser a -> [Token] -> [(a, [Token])]
parse (Parser p) = p

-- | Parses one token.
item :: Parser Token
item = Parser (\case []     -> []
                     (x:xs) -> [(x,xs)])

-- | Parses one token if it is the same as
-- the given one.
expect :: Token -> Parser ()
expect token = do
  t <- item
  unless (t == token) mzero

-- | Type keywords.
-- e.g int
data AST_TYPE =
    TYPE_INT    -- int
  | TYPE_DOUBLE -- double
  deriving Show
parseType :: Parser AST_TYPE
parseType = do
  t <- item
  case t of
    Keyword Int -> return TYPE_INT
    _           -> mzero

-- | Infix operations.
-- e.g +
data AST_OPERATION =
    PLUS   -- +
  | MINUS  -- -
  | MULT   -- *
  | DIV    -- /
  deriving Show
parseOperation :: Parser AST_OPERATION
parseOperation = undefined

-- | Variable declarations.
-- e.g. int main
type AST_DECLARATION = (AST_TYPE, AST_IDENTIFIER)
parseDeclaration :: Parser AST_DECLARATION
parseDeclaration = do 
  vartype <- parseType
  varname <- parseIdentifier
  return (vartype, varname)

-- | Number literals.
-- e.g. 2.5
newtype AST_LITERAL = AST_LITERAL Literal
parseLiteral :: Parser AST_LITERAL
parseLiteral = do
  t <- item
  case t of
    Literal l -> return (AST_LITERAL l)
    _         -> mzero

-- | Identifiers (names).
-- e.g. main
newtype AST_IDENTIFIER = AST_IDENTIFIER String
  deriving Show
parseIdentifier :: Parser AST_IDENTIFIER
parseIdentifier = do
  t <- item
  case t of
    Identifier i -> return (AST_IDENTIFIER i)
    _            -> mzero

-- | Function definitions.
-- e.g. int main() { 
--    println("hello world");
-- }
data AST_FUNCTION = 
  AST_FUNCTION 
    AST_TYPE          -- Return type
    AST_IDENTIFIER    -- Function name
    [AST_DECLARATION] -- Arguments
    [AST_STATEMENT]   -- Body
  deriving Show
parseFunction :: Parser AST_FUNCTION
parseFunction = do
  fntype <- parseType
  fnname <- parseIdentifier
  expect LParenthesis
  fnargs <- parseArgs <|> parseEmptyArgs
  expect LCurlyBrace
  expect RCurlyBrace

  return $ AST_FUNCTION
    fntype
    fnname 
    fnargs
    []

  where 
    parseEmptyArgs :: Parser [AST_DECLARATION]
    parseEmptyArgs = do
      expect RParenthesis
      return []

    parseArgs :: Parser [AST_DECLARATION]
    parseArgs = do
      declaration <- parseDeclaration
      xs <- (expect Comma >> parseArgs) 
        <|> parseEmptyArgs
      return (declaration:xs)

-- | Expressions.
-- e.g 4.0 + a
data AST_EXPRESSION =
    EXPR_LITERAL Literal
  | EXPR_INFIX_OPERATION AST_IDENTIFIER AST_OPERATION AST_IDENTIFIER
  deriving Show
parseExpression :: Parser AST_EXPRESSION
parseExpression = undefined

-- | Statements.
-- e.g. println("hello world");
data AST_STATEMENT =
    STMT_RETURN AST_EXPRESSION -- return <statement>;
  | STMT_RETURN_VOID           -- return;
  | STMT_EXPRESSION AST_EXPRESSION
  deriving Show
parseStatement :: [Token] -> Maybe (AST_STATEMENT, [Token])
parseStatement = undefined
