module Parser where

import Lexer

data AST
  = Node [AST]
  | Text token


parse :: [Token] -> AST
parse (token : remaining)
  = undefined
