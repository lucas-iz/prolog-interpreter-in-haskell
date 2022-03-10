import Type
import Aufgabe2
import Aufgabe3
import Aufgabe4
import Aufgabe5
import Aufgabe6
import Data.List

data SLDTree = SLDTree Goal [(Maybe Subst, SLDTree)]
   deriving Show

-- Creates a SLDTree
sld :: Prog -> Goal -> SLDTree
sld (Prog []) g = SLDTree g []
sld _ (Goal []) = SLDTree (Goal [Comb "" []]) []        
sld (Prog ps) (Goal (g:gs)) = SLDTree (Goal (g:gs)) (map (sldTupel (Prog ps)) (filter (\(a,_) -> a /= Nothing) (zip substs goals)))
   where
      substs = map (\(Rule r _) -> unify r g) (renameRules (allVars g) ps)
      goals = map (\(Rule r rs) -> Goal (map (apply (extract (unify r g))) rs)) (renameRules (allVars g) ps)

-- Renames the rules of a program.
renameRules :: [VarName] -> [Rule] -> [Rule]
renameRules vars rs = map (rename vars) rs 

-- Puts / Calls the 'sld'-function within a tupel.
sldTupel :: Prog -> (Maybe Subst, Goal) -> (Maybe Subst, SLDTree)
sldTupel p (a,b) = (a, sld p b)

instance Pretty SLDTree where
   pretty tree = prettyTree 0 tree
      where
         prettyTree n (SLDTree g tr) = intercalate "\n" 
                                     $ prettyIndented n g : map (\(Just a,b) -> prettyIndented2 (n+1) a ++ "\n" ++ prettyTree (n+1) b) tr
         prettyIndented 0 g = pretty g
         prettyIndented n g = concat (replicate (n - 1) "|   ") ++ "|   " ++ pretty g
         prettyIndented2 0 s = pretty s
         prettyIndented2 n s = concat (replicate (n - 1) "|   ") ++ "+-- " ++ pretty s


--- Testfälle ---
prog1 :: Prog
prog1 = Prog [Rule (Comb "ehemann" [Comb "monika" [], Comb "herbert" []]) [], Rule (Comb "ehemann" [Comb "anette" [], Comb "frank" []]) []]
goal1 :: Goal
goal1 = Goal [Comb "ehemann" [Comb "monika" [], Var (VarName "A")]]

prog2 :: Prog
prog2 = Prog [Rule (Comb "p" [Var (VarName "X"), Var (VarName "Z")]) [Comb "q" [Var (VarName "X"), Var (VarName "Y")], Comb "p" [Var (VarName "Y"), Var (VarName "Z")]], Rule (Comb "p" [Var (VarName "X"), Var (VarName "X")]) [], Rule (Comb "q" [Comb "a" [], Comb "b" []]) []]
goal2 :: Goal
goal2 = Goal [Comb "p" [Var (VarName "S"), Comb "b" []]]

prog3 :: Prog
prog3 = Prog [Rule (Comb "append" [Comb "[]" [], Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]
goal3 :: Goal
goal3 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Comb "." [Comb "1" [], Comb "[]" []]]]


type Strategy = SLDTree -> [Subst]

-- dfs :: Strategy   -- Tiefensuche
-- Durchlaufe Baum und liefere alle Ergebnisse.

-- bfs :: Strategy   -- Breitensuche
-- Durchlaufe Baum und liefere alle Ergebnisse.

-- solveWith :: Prog -> Goal -> Strategy -> [Subst]