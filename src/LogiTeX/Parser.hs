module Parser where

import Lexer

data AST
  = Root [AST]
  | Node Token [AST]
  | Leaf Token
    deriving (Show, Eq)

class Precedable a where
  precedence :: a -> Int

instance Precedable AST where
  precedence (Node token _)
    = precedence token
  precedence (Leaf token)
    = precedence token

instance Precedable Token where
  precedence (Token Question _)       = 3
  precedence (Token SubQuestion _)    = 2
  precedence (Token SubSubQuestion _) = 1
  precedence _                        = 0

precedes :: (Precedable a, Precedable b) => a -> b -> Bool
precedes x y
  = (precedence x) > (precedence y)

push :: AST -> AST -> AST
push (Leaf token) tree
  = Node token [tree]
push (Node token subTrees) tree
  = Node token (tree : subTrees)
push (Root subTrees) tree
  = Root $ tree : subTrees

parse :: [Token] -> AST
parse
  = parse' $ Root []
    where
      parse' :: AST -> [Token] -> AST
      parse' root []
        = case root of
            (Root (t1 : t2 : remaining))
              | t2 `precedes` t1 -> parse' (Root $ push t2 t1 : remaining) []
            _                    -> root
      parse' (Root trees@(t1 : t2 : remaining)) (token : tokens)
        | t2 `precedes` t1 && (not $ t1 `precedes` token)
          = parse' (Root $ push t2 t1 : remaining) $ token : tokens
      parse' (Root trees) (token : tokens)
        = parse' (Root $ Leaf token : trees) tokens
