module Unification where
import Types

type Substitution = [(String, Term)]

unify :: Term -> Term -> Substitution -> Maybe Substitution
unify (Constant a) (Constant b) subs                                                  -- Unification of constant
    | a == b = Just subs
    | otherwise = Nothing
unify (Variable v) term subs = unifyVar v term subs                                  -- Unification of variable
unify term (Variable v) subs = unifyVar v term subs
unify (Compound name1 args1) (Compound name2 args2) subs
    | name1 == name2 && length args1 == length args2 = unifyArgs args1 args2 subs    -- Unification of compounds
    | otherwise = Nothing
unify _ _ _= Nothing                                                                 -- Everything else

-- Unification of variables
unifyVar :: String -> Term -> Substitution -> Maybe Substitution
unifyVar var term subs
    | Just bound <- lookup var subs = unify bound term subs
    | occursCheck var term = Nothing
    | otherwise = Just ((var, term) : subs)

-- Checks for cyclic terms
occursCheck :: String -> Term -> Bool
occursCheck var (Variable v) = var == v
occursCheck var (Compound _ args) = any (occursCheck var) args
occursCheck _ _ = False

-- Unification of multiple terms
unifyArgs :: [Term] -> [Term] -> Substitution -> Maybe Substitution
unifyArgs [] [] subs = Just subs
unifyArgs (t1 : tx1) (t2 : tx2) subs = do
    subs' <- unify t1 t2 subs
    unifyArgs tx1 tx2 subs'
unifyArgs _ _ _ = Nothing

-- Unification of an atom
unifyAtom :: Atom -> Atom -> Substitution -> Maybe Substitution
unifyAtom (MkAtom name1 args1) (MkAtom name2 args2) subs
    | name1 == name2 = unifyArgs args1 args2 subs
    | otherwise = Nothing

-- Unification of a fact
unifyFact :: Atom -> Fact -> Maybe Substitution
unifyFact (MkAtom atomName atomArgs) (MkAtom factName factArgs)
    | atomName == factName = unifyArgs factArgs atomArgs []
    | otherwise = Nothing

filterRelevantSubs :: [String] -> Substitution -> Substitution
filterRelevantSubs queryVars = filter (\(vars, _) -> vars `elem` queryVars)