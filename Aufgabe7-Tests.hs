{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe2
import Aufgabe3 (Vars(allVars))
import Aufgabe4
import Aufgabe5
import Aufgabe6
unnoetig :: String 
unnoetig = pretty (Var (VarName "A"))
unnoetig2 :: [VarName]
unnoetig2 = allVars (Var (VarName "A"))

-- data SLDTree = Knoten (Maybe Goal) | Zweige [(Maybe Subst, SLDTree)]
data SLDTree = SLDTree Goal [(Maybe Subst, SLDTree)] | Empty
   deriving Show

sld :: Prog -> Goal -> SLDTree
sld (Prog []) g = SLDTree g [] -- Wir haben kein Programm :(
sld _ (Goal []) = SLDTree (Goal [Comb "" []]) []        -- Wir haben keine Anfrage :(

sld (Prog ps) (Goal (g:gs)) = SLDTree (Goal (g:gs)) (map (sldTupel (Prog ps)) (filter noNothing (zip substs goals)))
   where
      substs = map (\(Rule r _) -> unify r g) (renameRules (allVars g) ps)
      goals = map (\(Rule r rs) -> Goal (map (apply (extract (unify r g))) rs)) (renameRules (allVars g) ps)

filterRule :: Term -> Rule -> Bool 
filterRule t (Rule r _) = callTest t r

noNothing :: (Maybe Subst, Goal) -> Bool
noNothing (a,_) = a /= Nothing

sldTupel :: Prog -> (Maybe Subst, Goal) -> (Maybe Subst, SLDTree)
sldTupel p (a,b) = (a, sld p b)

prog :: Prog
prog = Prog [Rule (Comb "ehemann" [Comb "monika" [], Comb "herbert" []]) [], Rule (Comb "ehemann" [Comb "monika" [], Comb "frank" []]) []]
goal :: Goal
goal = Goal [Comb "ehemann" [Comb "monika" [], Var (VarName "A")]]

-- anfrage :: Term
-- anfrage = Comb "append" [Var (VarName "B"),Var (VarName "Y"),Comb "[]" []]
-- prog :: Prog
-- prog = Prog [Rule (Comb "append" [Comb "[]" [], Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]
-- goal :: Goal
-- goal = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Comb "." [Comb "1" [], Comb "[]" []]]]

testMap :: Prog -> Term -> [Maybe Subst]
testMap (Prog ps) g = map (\(Rule r _) -> unify r g) (renameRules (allVars g) ps)

testTerms :: Prog -> Term -> [Goal]
testTerms (Prog ps) g = map (\(Rule r rs) -> Goal (map (apply (extract (unify r g))) rs)) (renameRules (allVars g) ps)

renameRules :: [VarName] -> [Rule] -> [Rule]
renameRules vars rs = map (rename vars) rs 

testunify :: Term -> Term -> Maybe Subst
testunify t1 t2 = unify t1 t2


-- sld (Prog ps) (Goal gs) = Zweige (map (funk (Prog ps)) gs)
-- sld (Prog (Rule r l:ps)) (Goal gs) | l == []   = Zweige [()]
--                                    | otherwise = Zweige (map (funk (Prog ps)) gs)

funk :: Prog -> Term -> (Maybe Subst, SLDTree)
-- test if testUnify has result:
-- if yes, proceed
-- if no, call funk without first rule
-- funk (Prog []) _ = (Nothing, Knoten Nothing)
funk (Prog ((Rule r l):rs)) t | callTest r t = (unify r t, sld (Prog (Rule r l:rs)) (Goal (map (apply (extract (unify r t))) l)))
                              | otherwise     = funk (Prog rs) t

callTest :: Term -> Term -> Bool 
callTest (Comb r rs ) (Comb t ts) = testUnify (Comb r rs) (Comb t ts) 
callTest _ _ = False

testUnify :: Term -> Term -> Bool 
testUnify (Var _) _ = True
testUnify _ (Var _) = True
testUnify (Comb r []) (Comb t []) = r == t
testUnify (Comb r (a:as)) (Comb t (b:bs)) = r == t && testUnify a b && testUnify (Comb r as) (Comb t bs)

-- prog :: Prog
-- prog = Prog [Rule (Comb "ehemann" [Comb "monika" [], Comb "herbert" []]) [], Rule (Comb "ehemann" [Comb "monika" [], Comb "frank" []]) []]
-- goal :: Goal
-- goal = Goal [Comb "ehemann" [Comb "monika" [], Var (VarName "A")]]

instance Pretty SLDTree where
   pretty Empty = ""
   pretty (SLDTree g []) = pretty g
   pretty (SLDTree g ((s,tr):xs)) = pretty g ++ "\n+-- " ++ pretty (extract s) ++ "\n|   " ++ pretty tr ++ "\n" ++ pretty (SLDTree g xs)

-- instance Pretty (Rose a) where
--   pretty t = prettyTree 0 t
--     where
--     prettyTree n (Rose x ts) = intercalate "\n"
--                              $ prettyIndented n x : map (prettyTree (n + 1)) ts
--     prettyIndented 0 x = pretty x
--     prettyIndented n x = concat (replicate (n - 1) "|   ") ++ "+-- " ++ pretty x

term1 :: Term
term1 = Comb "p" [Var (VarName "X"), Var (VarName "Z")]
term2 :: Term
term2 = Comb "p" [Var (VarName "X"), Var (VarName "X")]
term3 :: Term
term3 = Comb "q" [Comb "a" [], Comb "b" []]

term4 :: Term
term4 = Comb "p" [Var (VarName "S"), Comb "b" []]

program :: Prog
program = Prog [rule1, rule2, rule3]

rule1 :: Rule
rule1 = Rule term1 [Comb "q" [Var (VarName "X"), Var (VarName "Y")], Comb "p" [Var (VarName "Y"), Var (VarName "Z")]]

rule2 :: Rule
rule2 = Rule term2 []

rule3 :: Rule
rule3 = Rule term3 []

goal2 :: Goal
goal2 = Goal [Comb "p" [Var (VarName "S"), Comb "b" []]]