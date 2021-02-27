module Compiler where
    
import Lexer
import Parser

compile :: String -> AST
compile = parser . lexer
