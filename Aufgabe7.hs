{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe4
import Aufgabe5
import Aufgabe6
data SLDTree = Knoten (Maybe Goal) | Zweige [(Maybe Subst, SLDTree)]

sld :: Prog -> Goal -> SLDTree
sld (Prog []) _ = Knoten Nothing -- Wir haben kein Programm :(
sld _ (Goal []) = Knoten Nothing -- Wir haben keine Anfrage :(
-- sld (Prog ((Rule r rt) : rs)) (Goal [t]) = Zweig [(unify r t, apply (extract (unify r t)) t)]

-- sld (Prog ((Rule r rt):rs)) (Goal (t:ts)) = sld (Prog ((Rule r rt):rs)) (Goal (testaufruf (Rule r rt) (Goal [t])))
   

mapTerms :: Term -> [Term] -> Term -> [Term]
mapTerms r rt t = map (apply (extract (unify r t))) rt

test1 :: Rule -> Goal -> [Term]
test1 (Rule r rt) (Goal (t:_)) = let (Rule _ nt) = rename [] (Rule r rt)
                                 in mapTerms r nt t

test2 :: Prog -> Goal -> [[Term]]
test2 (Prog rules) go = map (`test1` go) rules






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



