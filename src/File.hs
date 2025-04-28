module File where

import Parser
import Resolution
import Unification
import Types

import Data.List (intercalate)

-- Parse Prolog file into Context
parseContext :: String -> Maybe Context
parseContext input = parseLines (lines input) []
    where
        parseLines :: [String] -> Context -> Maybe Context
        parseLines [] context = Just context
        parseLines (line : rest) context =
            case parseFactOrRule line of
                Just (factOrRule, "") -> parseLines rest (factOrRule : context)
                _ -> Nothing  -- Parsing error

-- Read a query from the console
queryReader :: Context -> IO ()
queryReader context = do
    putStrLn "Enter your query (or type exit to quit) : "
    queryInput <- getLine
    if queryInput == "exit"
        then putStrLn "Goodbye!"
        else do
            case parseAtom queryInput of
                Just (query, "") -> do
                    let solution = resolve query context
                    if null solution
                        then putStrLn "No solution."
                        else mapM_ printSubs solution
                _ -> putStrLn "Invalid query!"
            queryReader context

-- Nice print of a substitution
printSubs :: Substitution -> IO ()
printSubs subs = do
    putStrLn "Substitutions:"
    mapM_ printPair subs
    putStrLn ""  -- Add an empty line after each substitution set
  where
    printPair :: (String, Term) -> IO ()
    printPair (var, term) = putStrLn $ "  " ++ var ++ " = " ++ showTerm term
    
    -- Helper function to show terms in a more Prolog-like way
    showTerm :: Term -> String
    showTerm (Constant name) = name
    showTerm (Variable name) = name
    showTerm (Compound functor args) = 
        functor ++ 
        (if null args then "" else "(" ++ intercalate ", " (map showTerm args) ++ ")")