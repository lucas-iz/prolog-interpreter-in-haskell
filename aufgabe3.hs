import Type

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var (VarName x)) = [VarName x]
    allVars (Comb _ []) = []
    allVars (Comb _ t) = allVarsH t
     where
        allVarsH [] = [] 
        allVarsH (Var (VarName x) : xs) =  [VarName x] ++ allVarsH xs
        allVarsH ((Comb _ []) : xs) = allVarsH xs
        allVarsH ((Comb _ t1) : xs) = allVarsH t1 ++ allVarsH xs



