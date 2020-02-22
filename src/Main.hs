import Data.Char          (isSpace)
import Data.List          (dropWhileEnd)
import System.Environment

conversionMap :: [(String, String)]
conversionMap
  = [ ("\\A", "\\forall")
    , ("\\E", "\\exists")
    , ("in",  "\\in")
    , ("and", "\\land")
    , ("not", "\\neg")
    , ("=>", " \\implies")
    , ("<=>", "\\iff")
    , ("~=", "\\approx")
    , (":=", "\\triangleq")
    , ("<=", "\\leqslant")
    , (">=", "\\geqslant")
    ]

isPrefix :: String -> String -> Bool
isPrefix "" _
  = True
isPrefix (_ : _) ""
  = False
isPrefix (x : xs) (y : ys)
  = x == y && isPrefix xs ys

replace :: [String] -> (String, String) -> [String]
replace text (keyword, replacement)
  = map replace' text
    where
      replace' :: String -> String
      replace' word
        | word == keyword = replacement
        | otherwise       = word

embedFile :: String -> String
embedFile text
  = fileFront ++ text ++ fileEnd
    where
      fileFront
        = "\\documentclass{article}\n\
          \\\usepackage{dsfont, amssymb}\n\
          \\\usepackage{listings}\n\
          \\\begin{document}\n\
          \\\begin{flushleft}\n"
      fileEnd
        = "\\end{flushleft}\n\
          \\\end{document}"

embedLine :: String -> String
embedLine text
  | text' == "" = "\\newline\\newline"
  | otherwise   = eqntFront ++ text' ++ eqntEnd
    where
      text'
        = dropWhileEnd isSpace $ dropWhile isSpace text
      eqntFront
        = "\\begin{equation}\n"
      eqntEnd
        = "\n\\end{equation}"

convertLine :: String -> String
convertLine ('#' : ' ' : text)
  = text
convertLine line
  = embedLine $ unwords $ foldl replace (words line) conversionMap

convert :: String -> String
convert text
  = embedFile $ unlines $ map convertLine $ lines text

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

usage :: IO ()
usage
  = do
    putStrLn "Invalid arguments"
    putStrLn "Usage: logitex [filename] [output]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                      -> pipedInput
    [inputFile]             -> fileInput inputFile
    [inputFile, outputFile] -> fileInputOutput inputFile outputFile
    _                       -> usage
