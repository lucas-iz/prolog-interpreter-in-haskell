import Type

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var (VarName x)) = [VarName x]
    allVars (Comb x []) = []

