import LogiTeX

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
