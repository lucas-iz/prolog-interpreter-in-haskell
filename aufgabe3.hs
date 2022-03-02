import Type

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var (VarName x)) = [VarName x]
    allVars (Comb _ []) = []
    allVars (Comb _ t) = nubV (allVarsH t)
     where
        allVarsH [] = [] 
        allVarsH (Var (VarName x) : xs) =  VarName x : allVarsH xs
        allVarsH ((Comb _ []) : xs) = allVarsH xs
        allVarsH ((Comb _ t1) : xs) = allVarsH t1 ++ allVarsH xs



instance Vars Rule where
  allVars (Rule t []) = allVars t
  allVars (Rule t ts) = allVars t ++ sub ts
    where
      sub [] = []
      sub (x : xs) = allVars x ++ sub xs

instance Vars Prog where
  allVars (Prog []) = []
  allVars (Prog (p : ps)) = allVars p ++ allVars (Prog ps)









  -- Remove all duplicates in a list of integers.
nubV :: [VarName] -> [VarName]
nubV []     = []
nubV (x:xs) = x : nubV (filter (/=x) xs)
