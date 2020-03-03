module Parser where

import Lexer

data AST
  = Node Token [AST]
  | Text Token

astPrecedence :: AST -> Int
ast (Node token _)
  = tokenPrecedence token
ast (Text token)
  = tokenPrecedence token

tokenPrecedence :: Token -> Int
tokenPrecedence (Token sectionType _)
  = precedence sectionType

precedence :: SectionType -> Int
precedence Question       = 3
precedence SubQuestion    = 2
precedence SubSubQuestion = 1
precedence _              = 0

parse :: [Token] -> AST
parse (token : remaining)
  = undefined
