import Type
import Aufgabe3

data Subst = Subst [(VarName, Term)] | Empty
  deriving (Show)

-- {A->B} // (a,b)

domain :: Subst -> [VarName]
--domain (Subst (a, b)) = filter (/= a) (allVars b)
domain Empty = []
domain (Subst []) = []
domain (Subst ((a, b) : rest)) = filter (/= a) (allVars b) ++ domain (Subst rest)

-- Aufruf: domain(Subst (VarName "c", Comb "1" [Var (VarName "b"), Var (VarName "c")]))

empty :: Subst
empty = Empty

single :: VarName -> Term -> Subst
single a b = Subst [(a, b)]

apply :: Subst -> Term -> Term
apply Empty t = t
apply (Subst []) t = t

-- apply (Subst (x:xs)) (Var name) = ???
-- apply (Subst (x:xs)) (Comb _ list) = ???

-- [(a,b),(c,d),(e,f)] (Var (VarName "A"))

-- Var VarName
-- Comb CombName [Term]