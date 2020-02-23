module Utils where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

takeFirstWord :: String -> String
takeFirstWord
  = takeWhile (not . isSpace)

trim :: String -> String
trim
  = dropWhileEnd isSpace . dropWhile isSpace

startsWith :: String -> String -> Bool
startsWith prefix text
  = prefix == (take (length prefix) text)

splitOn :: String -> String -> (String, String)
splitOn _ ""
  = ("", "")
splitOn delimiter text@(c : cs)
  | startsWith delimiter text = ("", drop (length delimiter) text)
  | otherwise                 = (c : before, after)
    where
      (before, after)
        = splitOn delimiter cs
