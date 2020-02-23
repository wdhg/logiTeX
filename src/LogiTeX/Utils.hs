module Utils where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

takeFirstWord :: String -> String
takeFirstWord
  = takeWhile (not . isSpace)

trim :: String -> String
trim
  = dropWhileEnd isSpace . dropWhile isSpace
