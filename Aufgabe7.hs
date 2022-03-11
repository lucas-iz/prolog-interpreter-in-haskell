{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Aufgabe7 where
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
sld1 p g v = SLDTree g (map (mapSLD p v) (filterNothing (list p g v)))

mapSLD :: Prog -> [VarName] -> (Maybe Subst, Goal) -> (Maybe Subst, SLDTree)
mapSLD p v (a,b) = (a, sld1 p b (v ++ substToVarName a))

filterNothing :: [(Maybe Subst, a)] -> [(Maybe Subst, a)]
filterNothing ls = filter (\(a,_) -> a /= Nothing) ls

list :: Prog -> Goal -> [VarName] -> [(Maybe Subst, Goal)]
list (Prog ps) (Goal (g:gs)) v = zip substs goals
   where
      substs = map (\(Rule r _) -> unify r g) (renameRules (allVars g ++ v) ps)
      goals  = map (\(Rule r rs) -> Goal (mapApply (extract (unify r g)) (rs ++ gs))) (renameRules (allVars g ++ v) ps)

mapApply :: Subst -> [Term] -> [Term]
mapApply s ts = map (apply s) ts

-- Renames the rules of a program.
renameRules :: [VarName] -> [Rule] -> [Rule]
renameRules vars rs = map (rename vars) rs

substToVarName :: Maybe Subst -> [VarName]
substToVarName Nothing = []
substToVarName (Just (Subst [])) = []
substToVarName (Just (Subst ((a,_):xs))) = a : substToVarName (Just (Subst xs))

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
goal4 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Var (VarName "Z")]]


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

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = s (sld p g)


bfs :: Strategy   -- Breitensuche
bfs (SLDTree _ []) = []
bfs (SLDTree g xs) = map extract (map (\(a,b) -> a) (leaves xs)) -- Substitutionen aller Blätter
                        ++ concat (map bfs (map knotToTree (notLeaves xs)))

leaves :: [(Maybe Subst, SLDTree)] -> [(Maybe Subst, SLDTree)]
leaves [] = []
leaves ((s,t) : rest) | isLeaf t = (s,t) : leaves rest
                      | otherwise = leaves rest

notLeaves :: [(Maybe Subst, SLDTree)] -> [(Maybe Subst, SLDTree)]
notLeaves [] = []
notLeaves ((s,t) : rest) | isLeaf t  = notLeaves rest
                         | otherwise = (s,t) : notLeaves rest

knotToTree :: (Maybe Subst, SLDTree) -> SLDTree
knotToTree (Nothing, t) = t
knotToTree (s, t) = t







-- neue Tests ---

progA :: Prog
progA = Prog [Rule (Comb "p" [Var (VarName "A"),Var (VarName "B")]) [Comb "a" [Var(VarName "A")], Comb "b" [Var (VarName "B")]],Rule (Comb "q" []) [Comb "p" [Comb "_" [], Comb "_" []]],Rule (Comb "a" [Comb "a" []]) [],Rule (Comb "b" [Comb "b" []]) []]

goalA1 :: Goal
goalA1 = Goal [Comb "p" [Var (VarName "A"), Var (VarName "B")]]
goalA2 :: Goal
goalA2 = Goal [Comb "p" [Comb "_" [], Comb "_" []]]
goalA3 :: Goal
goalA3 = Goal [Comb "q" []]


