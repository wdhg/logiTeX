import System.Environment

conversionMap :: [(String, String)]
conversionMap
  = [ ("\\A", "\\forall")
    , ("\\E", " \\exists ")
    , (" in ",  " \\in ")
    , (" and ", " \\land ")
    , (" => ", " \\implies")
    , (" <=> ", " \\iff")
    , ("~=", "\\approx")
    ]

isPrefix :: String -> String -> Bool
isPrefix "" _
  = True
isPrefix (_ : _) ""
  = False
isPrefix (x : xs) (y : ys)
  = x == y && isPrefix xs ys

replace :: String -> (String, String) -> String
replace "" _
  = ""
replace text@(c : remaining) mapping@(prefix, replacement)
  | isPrefix prefix text = replacement ++ text'
  | otherwise            = c : replace remaining mapping
    where
      text'
        = drop (length prefix) text

embedFile :: String -> String
embedFile text
  = fileFront ++ text ++ fileEnd
    where
      fileFront
        = "\\documentclass{article}\n\
          \\\usepackage{dsfont}\n\
          \\\usepackage{listings}\n\
          \\\begin{document}\n"
      fileEnd
        = "\\end{document}"

embedEqnt :: String -> String
embedEqnt text
  = eqntFront ++ text ++ eqntEnd
    where
      eqntFront
        = "\\begin{equation}\n"
      eqntEnd
        = "\n\\end{equation}"

convert :: String -> String
convert text
  = embedFile $ unlines $ map embedEqnt $ lines $ foldl replace text conversionMap

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
