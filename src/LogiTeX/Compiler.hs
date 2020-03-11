module Compiler where

import Lexer
import Parser

header :: String
header
  = "\\documentclass{article}\n\
    \\\usepackage{dsfont, amssymb, amsmath}\n\
    \\\usepackage{listings}\n\
    \\\begin{document}\n\
    \\\begin{flushleft}\n\
    \\\begin{enumerate}\n"

footer :: String
footer
  = "\\end{enumerate}\n\
    \\\end{flushleft}\n\
    \\\end{document}"

(==>) :: a -> b -> (a, b)
(==>)
  = (,)

wrappings :: [(SectionType, (String, String))]
wrappings
  = [ Comment ==>
      ( ""
      , ""
      )
    , Title ==>
      ( "\\title{"
      , "}\n\\date{}\n"
      )
    , Question ==>
      ( "\\item\\begin{enumerate}\n"
      , "\\end{enumerate}\n"
      )
    , SubQuestion ==>
      ( "\\item\\begin{enumerate}\n"
      , "\\end{enumerate}\n"
      )
    , SubSubQuestion ==>
      ( "\\item\n"
      , "\n"
      )
    , Equation ==>
      ( "\\begin{equation}\n"
      , "\\end{equation}\n"
      )
    , EquationMulti ==>
      ( "\\begin{align}\\begin{split}\n"
      , "\n\\end{split}\\end{align}\n"
      )
    , Snippet ==>
      ( "\\begin{lstlisting}\n"
      , "\n\\end{lstlisting}\n"
      )
    , Latex ==>
      ( ""
      , "\n"
      )
    , LatexMulti ==>
      ( ""
      , "\n"
      )
    , Text ==>
      ( ""
      , "\n"
      )
    ]

type Mapping
  = ([String], String)

type Mappings
  = [Mapping]

globalMappings :: Mappings
globalMappings
  = [ ["<=>"] ==> "\\iff"
    , ["=>"]  ==> "\\implies"
    , ["/="]  ==> "\\neq"
    , ["~="]  ==> "\\approx"
    , [":="]  ==> "\\triangleq"
    , ["<="]  ==> "\\leqslant"
    , [">="]  ==> "\\geqslant"
    ]

logicMappings :: Mappings
logicMappings
  = [ ["%"] ==> ""
    , ["\\A", "forall"] ==> "\\forall"
    , ["\\E", "exists"] ==> "\\exists"
    , ["in"] ==>  "\\in"
    , ["&&", "and"] ==> "\\land"
    , ["||", "or"] ==> "\\lor"
    , ["not"] ==> "\\neg"
    , ["precedes"] ==> "\\prec"
    ]

getMappings :: SectionType -> [Mappings]
getMappings sectionType
  | sectionType `elem` logicSections = [logicMappings, globalMappings]
  | otherwise                        = [globalMappings]
    where
      logicSections
        = [Equation, EquationMulti, Latex, LatexMulti]

applyMapping :: String -> Mapping -> String
applyMapping text (keywords, replacement)
  = foldl applyMapping' text keywords
    where
      applyMapping' :: String -> String -> String
      applyMapping' "" _
        = ""
      applyMapping' text'@(c : cs) keyword
        | before == keyword = replacement ++ (applyMapping' after keyword)
        | otherwise         = c : applyMapping' cs keyword
          where
            (before, after)
              = splitAt (length keyword) text'

replace :: Mappings -> String -> String
replace mappings text
  = foldl applyMapping text mappings

tokenToText :: Token -> String
tokenToText (Token Comment _)
  = ""
tokenToText (Token sectionType text)
  = foldr replace text $ getMappings sectionType

wrap :: SectionType -> String -> String
wrap sectionType text
  = case lookup sectionType wrappings of
      Nothing -> error $ "cannot find wrapping for " ++ show sectionType
      Just (before, after) -> before ++ text ++ after

-- convert strings
-- wrap strings in LaTeX
-- concat strings in order
compile :: AST -> String
compile (Leaf token@(Token sectionType text))
  = wrap sectionType $ tokenToText token
compile (Node token@(Token sectionType _) trees)
  = wrap sectionType $ concatMap compile $ reverse trees
compile (Root trees)
  = header ++ (concatMap compile $ reverse trees) ++ footer
