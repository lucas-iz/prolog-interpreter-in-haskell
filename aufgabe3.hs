import Type

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var (VarName x)) = [VarName x]
    allVars (Comb x []) = []

instance Vars Rule where
  allVars (Rule t []) = allVars t
  allVars (Rule t ts) = allVars t ++ sub ts
    where
      sub [] = []
      sub (x : xs) = allVars x ++ sub xs

instance Vars Prog where
  allVars (Prog []) = []
  allVars (Prog (p : ps)) = allVars p ++ allVars (Prog ps)