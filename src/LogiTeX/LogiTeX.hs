module LogiTeX where

import Lexer
import Parser

(==>) :: a -> b -> (a, b)
(==>)
  = (,)

fileBefore :: String
fileBefore
  = "\\documentclass{article}\n\
    \\\usepackage{dsfont, amssymb, amsmath}\n\
    \\\usepackage{listings}\n\
    \\\begin{document}\n\
    \\\begin{flushleft}\n\
    \\\begin{enumerate}\n"

fileAfter :: String
fileAfter
  = "\\end{enumerate}\n\
    \\\end{flushleft}\n\
    \\\end{document}"

sectionWrappings :: [(SectionType, (String, String))]
sectionWrappings
  = [ Title ==>
      ( "\\title{"
      , "}\n\\date{}\n"
      )
    , Question ==>
      ( "\\begin{enumerate}\n"
      , "\n\\end{enumerate}\n"
      )
    , SubQuestion ==>
      ( "\\begin{enumerate}\n"
      , "\n\\end{enumerate}\n"
      )
    , SubSubQuestion ==>
      ( ""
      , "\n"
      )
    , Equation ==>
      ( "\\item\\begin{equation}\n"
      , "\n\\end{equation}\n"
      )
    , EquationMulti ==>
      ( "\\item\\begin{align}\\begin{split}\n"
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

globalMappings :: [Mapping]
globalMappings
  = [ ["=>"] ==> " \\implies"
    , ["<=>"] ==> "\\iff"
    , ["/="] ==> "\\neq"
    , ["~="] ==> "\\approx"
    , [":="] ==> "\\triangleq"
    , ["<="] ==> "\\leqslant"
    , [">="] ==> "\\geqslant"
    ]

logicMappings :: [Mapping]
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

tokenToText :: Token -> String
tokenToText (Token Comment _)
  = ""
tokenToText (Token Question _)
  = ""
tokenToText (Token SubQuestion _)
  = ""
tokenToText (Token SubSubQuestion _)
  = ""
tokenToText (Token sectionType text)
  = case lookup sectionType sectionWrappings of
      Nothing              -> error "cannot find section in sectionWrappings"
      Just (before, after) -> before ++ text ++ after

replace :: [String] -> Mapping -> [String]
replace text (keywords, replacement)
  = map replace' text
    where
      replace' :: String -> String
      replace' word
        | word `elem` keywords = replacement
        | otherwise            = word

mapTokenText :: Token -> Token
mapTokenText token@(Token Snippet _)
  = token
mapTokenText (Token sectionType text)
  = Token sectionType $ unwords $ foldl replace text'' globalMappings
    where
      text'
        = words text
      logicSections
        = [Equation, EquationMulti, Latex, LatexMulti]
      text''
        | sectionType `elem` logicSections = foldl replace text' logicMappings
        | otherwise                        = text'

convert :: String -> String
convert text
  = fileBefore ++ innerText ++ fileAfter
    where
      innerText
        = concatMap tokenToText $ map mapTokenText $ tokenize text
pipedInput :: IO ()
pipedInput
  = interact convert

fileInput :: String -> IO ()
fileInput inputFile
  = do
    contents <- readFile inputFile
    putStrLn $ convert contents

fileInputOutput :: String -> String -> IO ()
fileInputOutput inputFile outputFile
  = do
    contents <- readFile inputFile
    writeFile outputFile $ convert contents
