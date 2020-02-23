import Data.Char          (isSpace)
import Data.List          (dropWhileEnd)
import System.Environment

type LineIdentifier
  = String -> Bool

type Embedding
  = (String, String)

type Mapping
  = ([String], String)

data LineType
  = LineType LineIdentifier Embedding [Mapping]

(==>) :: [String] -> String -> Mapping
(==>)
  = (,)

-- lineAll will map every line regardless of what it is
lineAll :: LineType
lineAll
  = LineType
    (const True)
    ("", "")
    [ ["=>"] ==> " \\implies"
    , ["<=>"] ==> "\\iff"
    , ["/="] ==> "\\neq"
    , ["~="] ==> "\\approx"
    , [":="] ==> "\\triangleq"
    , ["<="] ==> "\\leqslant"
    , [">="] ==> "\\geqslant"
    ]

lineLogic :: LineType
lineLogic
  = LineType
    ((== "%") . take 1)
    ( "\\item\n\
      \\\begin{align}\n\
      \\\begin{split}\n"
    ,  "\n\\end{split}\n\
       \\\end{align}"
    )
    [ ["%"] ==> ""
    , ["\\A", "forall"] ==> "\\forall"
    , ["\\E", "exists"] ==> "\\exists"
    , ["in"] ==>  "\\in"
    , ["&&", "and"] ==> "\\land"
    , ["||", "or"] ==> "\\lor"
    , ["not"] ==> "\\neg"
    , ["precedes"] ==> "\\prec"
    ]

lineTypes :: [LineType]
lineTypes
  = [lineAll, lineLogic]

trim :: String -> String
trim
  = dropWhileEnd isSpace . dropWhile isSpace

embedFile :: String -> String
embedFile text
  = fileStart ++ text ++ fileEnd
    where
      fileStart
        = "\\documentclass{article}\n\
          \\\usepackage{dsfont, amssymb, amsmath}\n\
          \\\usepackage{listings}\n\
          \\\begin{document}\n\
          \\\begin{flushleft}\n\
          \\\begin{enumerate}\n"
      fileEnd
        = "\\end{enumerate}\n\
          \\\end{flushleft}\n\
          \\\end{document}"

replace :: [String] -> Mapping -> [String]
replace text (keywords, replacement)
  = map replace' text
    where
      replace' :: String -> String
      replace' word
        | word `elem` keywords = replacement
        | otherwise            = word

embedLine :: Embedding -> String -> String
embedLine (beginning, end) line
  = beginning ++ line ++ end

convertLine :: String -> String
convertLine line
  = foldl convertLine' (trim line) lineTypes
    where
      convertLine' :: String -> LineType -> String
      convertLine' line (LineType isType embedding mappings)
        | isType line = embedLine embedding $ unwords $ foldl replace (words line) mappings
        | otherwise   = line


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
