module Parser where

import Data.Char (isLower, isUpper, isAlphaNum, isAlpha)
import Types ( Term(..), Atom(..), Fact(..), Rule (..))

-- Parsing an identifier
parseIdentifier :: String -> Maybe (String, String)
parseIdentifier (x : xs)
    | isLower x = let (name, rest) = span isAlphaNum xs in Just (x : name, rest)
    | otherwise = Nothing
parseIdentifier _ = Nothing

-- Parsing a variable
parseVariable :: String -> Maybe (String, String)
parseVariable (x : xs)
    | isUpper x = let (name, rest) = span isAlphaNum xs in Just (x : name, rest)
    | otherwise = Nothing
parseVariable _ = Nothing

-- Parsing a constant
parseConstant :: String -> Maybe (String, String)
parseConstant = parseIdentifier

-- Parsing a term
parseTerm :: String -> Maybe (Term, String)
parseTerm input = 
    case parseConstant input of
        Just (name, rest) -> parseCompoundOrConstant name rest
        Nothing -> case parseVariable input of
            Just (name, rest) -> Just (Variable name, rest)
            Nothing -> Nothing

parseCompoundOrConstant :: String -> String -> Maybe (Term, String)
parseCompoundOrConstant name ('(' : rest) = case parseArguments rest of
    Just (args, ')' : remaining) -> Just (Compound name args, remaining)
    _ -> Nothing
parseCompoundOrConstant name rest = Just (Constant name, rest)

parseArguments :: String -> Maybe ([Term], String)
parseArguments input = case parseTerm input of
    Just (term, rest) -> parseArguments' [term] rest
    Nothing -> Just ([], input)

parseArguments' :: [Term] -> String -> Maybe ([Term], String)
parseArguments' acc (',' : ' ' : rest) = case parseTerm rest of
    Just (term, remaining) -> parseArguments' (acc ++ [term]) remaining
    Nothing -> Nothing
parseArguments' acc rest = Just (acc, rest)

-- Parsing an atom
parseAtom :: String -> Maybe (Atom, String)
parseAtom input = case parseIdentifier input of
    Just (name, '(' : rest) -> case parseArguments rest of
        Just (args, ')' : remaining) -> Just (MkAtom name args, remaining)
        _ -> Nothing
    _ -> Nothing

-- Parsing a fact
parseFact :: String -> Maybe (Fact, String)
parseFact input = case parseAtom input of
    Just (atom, '.' : remaining) -> Just (atom, remaining)
    _ -> Nothing

-- Parsing a rule
parseRule :: String -> Maybe (Rule, String)
parseRule input = case parseAtom input of
    Just (headAtom, ' ' : ':' : '-' : ' ' : rest) -> case parseBody rest of
        Just (bodyAtoms, '.' : remaining) -> Just (MkRule headAtom bodyAtoms, remaining)
        _ -> Nothing
    _ -> Nothing

parseBody :: String -> Maybe ([Atom], String)
parseBody input = case parseAtom input of
    Just (atom, rest) -> parseBody' [atom] rest
    Nothing -> Nothing

parseBody' :: [Atom] -> String -> Maybe ([Atom], String)
parseBody' acc (',' : ' ' : rest) = case parseAtom rest of
    Just (atom, remaining) -> parseBody' (acc ++ [atom]) remaining
    Nothing -> Nothing
parseBody' acc rest = Just (acc, rest)

-- General Parser
parseFactOrRule :: String -> Maybe (Either Fact Rule, String)
parseFactOrRule input = case parseFact input of
    Just (fact, rest) -> Just (Left fact, rest)
    Nothing -> case parseRule input of
        Just (rule, rest) -> Just (Right rule, rest)
        Nothing -> Nothing