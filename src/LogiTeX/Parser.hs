module Parser where

import Lexer

data AST
  = Question [AST]
  | Text token


parse :: [Token] -> AST
parse
  = undefined
