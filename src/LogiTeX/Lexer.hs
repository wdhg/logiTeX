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
splitOnType text
  = (prefix, trim $ drop (length prefix) text)
    where
      prefix
        = fst $ head $ filter prefixMatches typeMap
      prefixMatches :: (String, SectionType) -> Bool
      prefixMatches (prefix, sectionType)
        = prefix == (take (length prefix) text)

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
getNextToken
  = undefined

tokenize :: String -> [Token]
tokenize
  = undefined
