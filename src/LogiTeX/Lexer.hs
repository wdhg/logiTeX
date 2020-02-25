module Lexer where

import Data.Char (isSpace)
import Utils

data SectionType
  = Title
  | Question
  | SubQuestion
  | SubSubQuestion
  | Equation
  | EquationMulti
  | Snippet
  | Latex
  | LatexMulti
  | Comment
  | Text
    deriving (Show, Eq)

data Token
  = Token SectionType String
    deriving (Show, Eq)

typeMap :: [(String, SectionType)]
typeMap
  = [ ("~"   , Title)
    , ("#"   , Question)
    , ("##"  , SubQuestion)
    , ("###" , SubSubQuestion)
    , ("%"   , Equation)
    , ("%%%" , EquationMulti)
    , ("```" , Snippet)
    , ("@"   , Latex)
    , ("@@@" , LatexMulti)
    , ("//"  , Comment)
    , (""    , Text)
    ]

splitOnType :: String -> (String, String)
splitOnType
  = undefined

getType :: String -> SectionType
getType prefix
  = case lookup prefix typeMap of
      Just sectionType -> sectionType
      Nothing          -> error $ "no SectionType for prefix " ++ prefix

getDelimiter :: SectionType -> String
getDelimiter EquationMulti = "%%%"
getDelimiter Snippet       = "```"
getDelimiter LatexMulti    = "@@@"
getDelimiter _             = "\n"

getNextToken :: String -> (Token, String)

tokenize :: String -> [Token]
tokenize
  = undefined
