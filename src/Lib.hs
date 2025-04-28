module Lib where
import File

main :: IO ()
main = do
    putStrLn "Enter path of prolog file: "
    filePath <- getLine
    content <- readFile filePath
    case parseContext content of
        Just context -> do
            putStrLn "File loaded successfully!"
            queryReader context
        Nothing -> putStrLn "Failed to open/parse the file!"