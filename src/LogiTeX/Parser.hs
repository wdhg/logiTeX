module Parser where

import Lexer

data AST
  = Root [AST]
  | Node Token [AST]
  | Leaf Token
    deriving (Show)

astPrecedence :: AST -> Int
astPrecedence (Node token _)
  = tokenPrecedence token
astPrecedence (Leaf token)
  = tokenPrecedence token

tokenPrecedence :: Token -> Int
tokenPrecedence (Token sectionType _)
  = precedence sectionType

precedence :: SectionType -> Int
precedence Question       = 3
precedence SubQuestion    = 2
precedence SubSubQuestion = 1
precedence _              = 0

push :: AST -> AST -> AST
push (Leaf token) tree
  = Node token [tree]
push (Node token subTrees) tree
  = Node token (tree : subTrees)
push (Root subTrees) tree
  = Root (tree : subTrees)

collate :: [AST] -> AST
-- PRE: trees are in assending order of precedence with the last tree having a
-- unique precedence level
collate
  = head . until equalPrecedence collate'
    where
      equalPrecedence :: [AST] -> Bool
      equalPrecedence (top : remaining)
        = all ((== level) . astPrecedence) remaining
          where
            level
              = astPrecedence top
      collate' :: [AST] -> [AST]
      collate' (top : remaining)
        = (foldl push nodeTree $ top : subTrees) : remaining'
          where
            level
              = astPrecedence top
            (subTrees, nodeTree : remaining')
              = span ((== level) . astPrecedence) remaining

parse :: [Token] -> AST
parse
  = undefined
