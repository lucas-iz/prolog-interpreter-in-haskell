module Aufgabe3 where
import Type
import Data.List

class Vars a where
    allVars :: a -> [VarName]

-- change complete (concat und map) 
-- change complete (using imported nub)
instance Vars Term where
    allVars (Var (VarName x)) = [VarName x]
    allVars (Comb _ []) = []
    allVars (Comb _ t) =  nub (concatMap allVars t)
  
instance Vars Rule where
  allVars (Rule t []) = allVars t
  allVars (Rule t ts) = nub (allVars t ++ concatMap allVars ts)
   
instance Vars Prog where
  allVars (Prog []) = []
  allVars (Prog (p : ps)) = nub (allVars p ++ allVars (Prog ps))

instance Vars Goal where
  allVars (Goal []) = []
  allVars (Goal (t : ts)) = nub (allVars t ++ allVars (Goal ts))




-- gives an infinite list of valid variable names
-- change complete (using map)
freshVars :: [VarName]
freshVars = map (\x -> VarName [x]) ['A' .. 'Z'] ++ map (\(x,y) -> VarName (y : show x)) ([(a, b) | a <- [0 ..], b <- ['A' .. 'Z']])

