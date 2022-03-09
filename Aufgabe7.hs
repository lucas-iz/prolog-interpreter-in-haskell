{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe2
-- import Aufgabe3
import Aufgabe4
import Aufgabe5
import Data.Maybe
import Aufgabe6
import Aufgabe3 (Vars(allVars))

-- data SLDTree = Knoten (Maybe Goal) | Zweige [(Maybe Subst, SLDTree)]
data SLDTree = SLDTree Goal [(Maybe Subst, SLDTree)] | Empty | Success
   deriving Show

sld :: Prog -> Goal -> SLDTree
sld (Prog []) g = SLDTree g [] -- Wir haben kein Programm :(
sld _ (Goal []) = Empty        -- Wir haben keine Anfrage :(
sld (Prog (r : rs)) (Goal [g]) = SLDTree (Goal [g]) [(subs, tree)]
    where
        subs = unify g (firstTermRule (rename (allVars g) r)) 
        tree | isNothing (unify (firstTermRule r) g) = Empty
             | isNothing (ds (firstTermRule r) g)  = Success
             | otherwise = sld (Prog (r : rs)) (Goal [apply (extract (unify (firstTermRule r) g)) g])

firstTermRule :: Rule -> Term 
firstTermRule (Rule t _) = t

a :: Rule
a = (rename (allVars goal) r)

r :: Rule
r = Rule (Comb "ehemann" [Comb "monika" [], Comb "herbert" []]) []
-- -- 
prog :: Prog
prog = Prog [Rule (Comb "ehemann" [Comb "monika" [], Comb "herbert" []]) [], Rule (Comb "ehemann" [Comb "monika" [], Comb "frank" []]) []]
goal :: Goal
goal = Goal [Comb "ehemann" [Comb "monika" [], Var (VarName "A")]]


prog1 :: Prog
prog1 = Prog [Rule (Comb "append" [Comb "[]" [], Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]
goal1 :: Goal
goal1 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Comb "." [Comb "1" [], Comb "[]" []]]]
g2 :: Goal
g2 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Var (VarName "Z")]]

-- hff :: [Rule] -> Goal -> [(Maybe Subst, SLDTree)]
-- hff ((Rule t1 (t : ts)) : rs) (Goal [g]) = [(unify t1 g, (sld ))]


-- goal = Goal [apply (extract (unify t1 g)) g]

-- bla = (hff ((Rule t1 (t : ts)) : rs) (Goal [g]))


progToListOfRules :: Prog -> [Rule]
progToListOfRules (Prog rs) = rs

-- unnoetig :: String 
-- unnoetig = pretty (Var (VarName "A"))

-- sld :: Prog -> Goal -> SLDTree
-- sld (Prog []) g = SLDTree g [] -- Wir haben kein Programm :(
-- sld _ (Goal []) = Empty        -- Wir haben keine Anfrage :(

-- sld (Prog ps) (Goal (g:gs)) = SLDTree (Goal (g:gs)) (zip substs (map (sld (Prog ps)) terms))
--    where 
--       substs = map (\(Rule r _) -> unify r g) (filter (filterRule g) ps)
--       terms = map (\(Rule r rs) -> Goal (map (apply (extract (unify r g))) rs)) (filter (filterRule g) ps)
--       -- terms = map (sld (Prog ps)) (goals)

-- filterRule :: Term -> Rule -> Bool 
-- filterRule t (Rule r _) = callTest t r


-- -- sld (Prog ps) (Goal gs) = Zweige (map (funk (Prog ps)) gs)
-- -- sld (Prog (Rule r l:ps)) (Goal gs) | l == []   = Zweige [()]
-- --                                    | otherwise = Zweige (map (funk (Prog ps)) gs)

-- funk :: Prog -> Term -> (Maybe Subst, SLDTree)
-- -- test if testUnify has result:
-- -- if yes, proceed
-- -- if no, call funk without first rule
-- -- funk (Prog []) _ = (Nothing, Knoten Nothing)
-- funk (Prog ((Rule r l):rs)) t | callTest r t = (unify r t, sld (Prog (Rule r l:rs)) (Goal (map (apply (extract (unify r t))) l)))
--                               | otherwise     = funk (Prog rs) t

-- callTest :: Term -> Term -> Bool 
-- callTest (Comb r rs ) (Comb t ts) = testUnify (Comb r rs) (Comb t ts) 
-- callTest _ _ = False

-- testUnify :: Term -> Term -> Bool 
-- testUnify (Var _) _ = True
-- testUnify _ (Var _) = True
-- testUnify (Comb r []) (Comb t []) = r == t
-- testUnify (Comb r (a:as)) (Comb t (b:bs)) = r == t && testUnify a b && testUnify (Comb r as) (Comb t bs)



-- test :: SLDTree
-- test = sld prog goal

-- term1 :: Term
-- term1 = Comb "p" [Var (VarName "X"), Var (VarName "Z")]
-- term2 :: Term
-- term2 = Comb "p" [Var (VarName "X"), Var (VarName "X")]
-- term3 :: Term
-- term3 = Comb "q" [Comb "a" [], Comb "b" []]

-- term4 :: Term
-- term4 = Comb "p" [Var (VarName "S"), Comb "b" []]




-- -- program :: Prog
-- -- program = Prog [rule1, rule2, rule3]


-- -- rule1 :: Rule
-- -- rule1 = Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [Comb "q" [Var (VarName "X"), Var (VarName "Y")], Comb "p" [Var (VarName "Y"), Var (VarName "Z")]]

-- -- rule2 :: Rule
-- -- rule2 = Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) []

-- -- rule3 :: Rule
-- -- rule3 = Rule (Comb "q" [Comb "a" [], Comb "b" []]) []

-- -- goal :: Goal
-- -- goal = Goal [Comb "p" [Var (VarName "S"), Comb "b" []]]