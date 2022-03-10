{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe2
import Aufgabe3
import Aufgabe4
import Aufgabe5
import Aufgabe6
import Data.List

data SLDTree = SLDTree Goal [(Maybe Subst, SLDTree)]
   deriving Show

sld :: Prog -> Goal -> SLDTree
sld p g = sld1 p g []
-- Creates a SLDTree
sld1 :: Prog -> Goal -> [VarName] -> SLDTree
sld1 (Prog []) g _ = SLDTree g [] 
sld1 _ (Goal []) _ = SLDTree (Goal [Comb "" []]) []
sld1 (Prog ps) (Goal (g:gs)) v = SLDTree (Goal (g:gs)) (map (sldTupel (Prog ps) v substs) (filter (\(a,_) -> a /= Nothing)  (zip substs goals)))
   where
      substs = map (\(Rule r _) -> unify r g) (renameRules (allVars g ++ v) ps)
      goals = map (\(Rule r rs) -> Goal (map (apply (extract (unify r g))) rs)) (renameRules (allVars g ++ v) ps)

substToVarName :: [Maybe Subst] -> [VarName]
substToVarName [] = []
substToVarName (Nothing : xs) = substToVarName xs
substToVarName ((Just (Subst [])) : _) = []
substToVarName ((Just (Subst ((v, _) : ss))) : r) = (v : substToVarName [Just (Subst ss)]) ++ substToVarName r

-- Renames the rules of a program.
renameRules :: [VarName] -> [Rule] -> [Rule]
renameRules vars rs = map (rename vars) rs

-- Puts / Calls the 'sld'-function within a tupel.
sldTupel :: Prog -> [VarName] -> [Maybe Subst] -> (Maybe Subst, Goal) -> (Maybe Subst, SLDTree)
sldTupel p v w (a,b)  = (a, sld1 p b (v ++ substToVarName w))

instance Pretty SLDTree where
   pretty tree = prettyTree 0 tree
      where
         prettyTree 6 _ = intercalate "\n" [concat (replicate 5 "|   ") ++ "|    ..."]
         prettyTree n (SLDTree g tr) = intercalate "\n"
                                     $ prettyIndented n g : map (\(Just a,b) -> prettyIndented2 (n+1) a ++ "\n" ++ prettyTree (n+1) b) tr
         prettyIndented 0 g = pretty g
         prettyIndented n g = concat (replicate (n - 1) "|   ") ++ "|   " ++ pretty g
         prettyIndented2 0 s = pretty s
         prettyIndented2 n s = concat (replicate (n - 1) "|   ") ++ "+-- " ++ pretty s


--- substToVarNamefälle ---
prog1 :: Prog
prog1 = Prog [Rule (Comb "ehemann" [Comb "monika" [], Comb "herbert" []]) [], Rule (Comb "ehemann" [Comb "monika" [], Comb "frank" []]) []]
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

goal4 :: Goal
goal4 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), (Var (VarName "Z"))]]


type Strategy = SLDTree -> [Subst]

-- dfs :: Strategy   -- Tiefensuche
-- Durchlaufe Baum und liefere alle Ergebnisse.

dfs :: Strategy
dfs (SLDTree g xs) = map (filterSubst (allVars g)) (dfs1 [] (SLDTree g xs))

filterSubst :: [VarName] -> Subst -> Subst
filterSubst [] _ = Subst []
filterSubst _ (Subst []) = Subst []
filterSubst v (Subst (s:ss)) | fst s `elem` v = Subst [s] `combine` filterSubst v (Subst ss)
                             | otherwise = filterSubst v (Subst ss)

combine :: Subst -> Subst -> Subst 
combine (Subst s1) (Subst s2) = Subst (s1++s2)

dfs1 :: [Subst] -> SLDTree -> [Subst]
dfs1 v (SLDTree _ xs) = map (viaCompose . mapf v) xs

mapf :: [Subst] -> (Maybe Subst, SLDTree) -> [Subst]
mapf s (a,b) | isLeaf b  = extract a : s
             | otherwise = dfs1 (extract a : s) b

viaCompose :: [Subst] -> Subst
viaCompose [] = Subst []
viaCompose (x:xs) = compose x (viaCompose xs)

isLeaf :: SLDTree -> Bool 
isLeaf (SLDTree (Goal [Comb g _]) _) = g == ""
isLeaf _ = False



bfs :: Strategy   -- Breitensuche
bfs (SLDTree g xs) = map (filterSubst (allVars g)) (dfs2 [] (SLDTree g xs))

dfs2 :: [Subst] -> SLDTree -> [Subst]
-- dfs2 v (SLDTree _ []) = 
dfs2 v (SLDTree _ xs) = map (viaCompose . mapf2 v) xs

mapf2 :: [Subst] -> (Maybe Subst, SLDTree) -> [Subst]
mapf2 s (a,b) | isLeaf b  = extract a : s
              | otherwise = [Subst []]




-- solveWith :: Prog -> Goal -> Strategy -> [Subst]