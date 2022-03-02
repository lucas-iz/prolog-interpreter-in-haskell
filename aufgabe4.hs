{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe3

data Subst = Single (VarName, Term) | Empty
  deriving (Show)

-- {A->B} // (a,b)

domain :: Subst -> [VarName]
domain (Single (a, b)) = filter (/= a) (allVars b)

-- Aufruf: domain(Single (VarName "c", Comb "1" [Var (VarName "b"), Var (VarName "c")]))

empty :: Subst
empty = Empty

single :: VarName -> Term -> Subst
single a b = Single (a, b)

-- apply :: Subst -> Term -> Term
-- apply Empty t = t
-- apply (Single (a,b)) t =

-- Var VarName
-- Comb CombName [Term]