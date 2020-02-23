module Lexer where

import Data.Char (isSpace)
import Utils     (takeFirstWord)

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
  | Text

data Token
  = Token SectionType String

getType :: String -> SectionType
getType "~"   = Title
getType "#"   = Question
getType "##"  = SubQuestion
getType "###" = SubSubQuestion
getType "%"   = Equation
getType "%%%" = EquationMulti
getType "```" = Snippet
getType "@"   = Latex
getType "@@@" = LatexMulti
getType _     = Text

getDelimiter :: SectionType -> String
getDelimiter EquationMulti = "%%%"
getDelimiter Snippet       = "```"
getDelimiter LatexMulti    = "@@@"
getDelimiter _             = "\n"

getNextToken :: String -> (Token, String)
getNextToken text
  = undefined
    where
      delimiter
        = getDelimiter sectionType
      sectionType
        = getType $ takeFirstWord text

tokenize :: String -> [Token]
tokenize
  = undefined
