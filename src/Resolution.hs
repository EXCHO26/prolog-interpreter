module Resolution where

import Types
import Unification
import Data.Either (partitionEithers)

type Context = [Either Fact Rule]

-- Evaluating a query against facts
matchFact :: Atom -> [Fact] -> [Substitution]
matchFact _ [] = []
matchFact query (fact : rest) =
    case unifyFact fact query of
        Just subs -> subs : matchFact query rest
        Nothing -> matchFact query rest

-- Evaluating a query against rules
matchRule :: Atom -> [Rule] -> Context -> [Substitution]
matchRule _ [] _ = []
matchRule query ((MkRule headAtom body) : rest) context =
    case unifyAtom headAtom query [] of
        Just subs -> let solution = resolveBody body context subs
                     in solution ++ matchRule query rest context
        Nothing -> matchRule query rest context

-- Resolving the body of an atom
resolveBody :: [Atom] -> Context -> Substitution -> [Substitution]
resolveBody [] _ subs = [subs]
resolveBody (atom : rest) context subs =
    let atomWithSubs = applySubstitution subs atom                 -- Apply substitution to the current atom
        atomSolutions = resolve atomWithSubs context               -- Resolve the current atom
    in concatMap (\subs' -> resolveBody rest context (subs' ++ subs)) atomSolutions

applySubstitution :: Substitution -> Atom -> Atom
applySubstitution subs (MkAtom name args) = MkAtom name (map (applySubstitutionToTerms subs) args)

applySubstitutionToTerms :: Substitution -> Term -> Term
applySubstitutionToTerms subs (Variable x) =                  -- Apply it to variables
    case lookup x subs of
        Just term -> term
        Nothing -> Variable x
applySubstitutionToTerms subs (Compound name args) =          -- Apply it to compounds
    Compound name (map (applySubstitutionToTerms subs) args)
applySubstitutionToTerms _ cons = cons                        -- Constant stay the same

-- General resolve function
resolve :: Atom -> Context -> [Substitution]
resolve query context =
    let allSubs = matchFact query facts ++ matchRule query rules context
        queryVars = varsInAtom query
    in map (filterRelevantSubs queryVars) allSubs
    where
        (facts, rules) = partitionEithers context