{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe4
import Aufgabe5
data SLDTree = Knoten Goal | Zweig [(Maybe Subst, SLDTree)] | Empty

sld :: Prog -> Goal -> SLDTree
sld (Prog []) _ = Empty -- Wir haben kein Programm :(
sld _ (Goal []) = Empty -- Wir haben keine Anfrage :(

sld (Prog ((Rule r _)  : rs)) (Goal [t]) = Zweig [(unify r t, apply (extract (unify r t)) t)]






program :: Prog
program = Prog [rule1, rule2, rule3]


rule1 :: Rule
rule1 = Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [Comb "q" [Var (VarName "X"), Var (VarName "Y")], Comb "p" [Var (VarName "Y"), Var (VarName "Z")]]

rule2 :: Rule
rule2 = Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) []

rule3 :: Rule
rule3 = Rule (Comb "q" [Comb "a" [], Comb "b" []]) []

goal :: Goal
goal = Goal [Comb "p" [Var (VarName "S"), Comb "b" []]]



