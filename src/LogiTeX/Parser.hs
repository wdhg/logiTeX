module Parser where

import Lexer

data AST
  = Node Token [Tree]
  | Leaf Token

tokenBinding :: Token -> Int
tokenBinding (Token sectionType _)
  = binding sectionType

-- the lower the value the more binding
binding :: SectionType -> Int
binding EquationMulti
  = 1
binding Snippet
  = 1
binding LatexMulti
  = 1
binding _
  = 0

parse :: [Token] -> AST
parse
  = undefined
