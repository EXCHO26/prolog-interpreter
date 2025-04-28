module Types where

data Term
    = Constant String
    | Variable String
    | Compound String [Term]
    deriving (Show, Eq)

data Atom = MkAtom String [Term]
    deriving (Show, Eq)

type Fact = Atom

data Rule = MkRule Atom [Atom]
    deriving (Show, Eq)

-- Cheching if an atom has variables
noVars :: Atom -> Bool
noVars (MkAtom _ terms) = all noVarsTerm terms

-- Cheching if a term has variables
noVarsTerm :: Term -> Bool 
noVarsTerm (Variable _) = False
noVarsTerm _ = True

-- Collecting all variables in an atom
varsInAtom :: Atom -> [String]
varsInAtom (MkAtom _ terms) = concatMap varsInTerm terms

-- Collecting all variables in a term
varsInTerm :: Term -> [String]
varsInTerm (Variable x) = [x]
varsInTerm (Constant _) = []
varsInTerm (Compound _ args) = concatMap varsInTerm args