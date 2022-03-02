module Aufgabe3 where
import Type

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var (VarName x)) = [VarName x]
    allVars (Comb _ []) = []
    allVars (Comb _ t) =  nubV (allVarsH t)
     where
        allVarsH [] = []
        allVarsH (Var (VarName x) : xs) = VarName x : allVarsH xs
        allVarsH ((Comb _ []) : xs) = allVarsH xs
        allVarsH ((Comb _ t1) : xs) = allVarsH t1 ++ allVarsH xs
instance Vars Rule where
  allVars (Rule t []) = allVars t
  allVars (Rule t ts) = nubV (allVars t ++ sub ts)
    where
      sub [] = []
      sub (x : xs) = allVars x ++ sub xs
instance Vars Prog where
  allVars (Prog []) = []
  allVars (Prog (p : ps)) = nubV (allVars p ++ allVars (Prog ps))
instance Vars Goal where
  allVars (Goal []) = []
  allVars (Goal (t : ts)) = nubV (allVars t ++ allVars (Goal ts))

-- Remove all duplicates in a list of integers.
nubV :: [VarName] -> [VarName]
nubV []     = []
nubV (x:xs) = x : nubV (filter (/=x) xs)



-- gives an infinite list of valid variable names
freshVars :: [VarName]
freshVars = charToVarName capitalLetters ++ tupelToVarName (crossproduct [0 ..] ['A' .. 'Z'])

-- list of A - Z
capitalLetters :: [Char]
capitalLetters = ['A' .. 'Z']

-- forms a Char into a VarName
charToVarName :: [Char] -> [VarName]
charToVarName [] = []
charToVarName (x : xs) = (VarName [x] : charToVarName xs)

-- forms a tupel into a VarName
tupelToVarName :: [(Int, Char)] -> [VarName]
tupelToVarName [] = []
tupelToVarName ((x1,x2) : xs) = VarName (x2 : show x1) : tupelToVarName xs

-- computes cross product of 2 lists
crossproduct :: [a] -> [b] -> [(a, b)]
crossproduct listeA listeB = [(a, b) | a <- listeA, b <- listeB]