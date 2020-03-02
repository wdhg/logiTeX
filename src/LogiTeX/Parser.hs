module Parser where

import Lexer

data AST
  = Node Token [AST]
  | Text Token


parse :: [Token] -> AST
parse (token : remaining)
  = undefined
