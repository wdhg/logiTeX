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
  | EmptyToken
    deriving (Show, Eq)

-- order of typeMap shows order of checks
typeMap :: [(String, SectionType)]
typeMap
  = [ ("~"   , Title)
    , ("###" , SubSubQuestion)
    , ("##"  , SubQuestion)
    , ("#"   , Question)
    , ("%%%" , EquationMulti)
    , ("%"   , Equation)
    , ("```" , Snippet)
    , ("@@@" , LatexMulti)
    , ("@"   , Latex)
    , ("//"  , Comment)
    , (""    , Text)
    ]

isQuestion :: SectionType -> Bool
isQuestion Question       = True
isQuestion SubQuestion    = True
isQuestion SubSubQuestion = True

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
getNextToken text
  = (Token sectionType $ trim tokenContent, trim remaining')
    where
      (prefix, remaining)
        = splitOnType text
      sectionType
        = getType prefix
      (tokenContent, remaining')
        = splitOn (getDelimiter sectionType) remaining

tokenize :: String -> [Token]
tokenize text
  | null $ trim text = []
  | otherwise        = token : tokenize remaining
    where
      (token, remaining)
        = getNextToken text
