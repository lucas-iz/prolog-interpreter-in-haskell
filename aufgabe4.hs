{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe3

-- {A->B} // (a,b)
data Subst = Subst [(VarName, Term)]
  deriving (Show)

-- Aufruf: domain(Subst (VarName "c", Comb "1" [Var (VarName "b"), Var (VarName "c")]))
domain :: Subst -> [VarName]
--domain (Subst (a, b)) = filter (/= a) (allVars b)
domain (Subst []) = []
domain (Subst ((a, b) : rest)) = filter (/= a) (allVars b) ++ domain (Subst rest)

empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single a b | a `elem` (allVars b) = Subst []          -- verhindert {A->A}
           | otherwise            = Subst [(a, b)]


apply :: Subst -> Term -> Term
apply (Subst []) t = t
apply (Subst [(a,b)]) (Var t) | a == t    = b
                              | otherwise = Var t
apply (Subst (r:rs)) (Var t)  = apply (Subst rs) (apply (Subst [r]) (Var t))
-- apply (Subst [(a,b)]) (Comb n (t:ts)) = apply (Subst [(a,b)]) (Comb n (ersetze:ts))
--    where 
--       ersetze = apply (Subst [(a,b)]) t)                               


-- apply (Subst [(VarName "I", Var (VarName "J")),(VarName "J", Var (VarName "H"))]) (Var (VarName "I"))

-- apply (Subst (x:xs)) (Comb _ list) = ???


-- Var VarName
-- Comb CombName [Term]