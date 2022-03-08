{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe2
import Aufgabe3
import Aufgabe4 
-- import Aufgabe5
import Data.List

rename :: [VarName] -> Rule -> Rule
rename vars (Rule t []) = Rule (apply (replace (allVars t) (validVars vars (Rule t []))) t) []
rename vars (Rule t ts) = Rule (apply (replace (allVars t) (validVars vars (Rule t ts))) t)
                               (map (\x -> apply (replace (allVars x) (validVars vars (Rule t ts))) x) ts)

validVars :: [VarName] -> Rule -> [VarName]
validVars vars rule = freshVars \\ (vars ++ allVars rule)

replace :: [VarName] -> [VarName] -> Subst
replace [] _ = empty
replace _ [] = empty
replace t v = Subst (map (\(a,b) -> (a, Var b)) (zip t v))

-- replaceTerm :: Term -> Term

rule1 :: Rule
rule1 = Rule term1 [term2, term3]
rule2 :: Rule
rule2 = Rule term1 []

term1 :: Term
term1 = Var (VarName "A")
term2 :: Term
term2 = Var (VarName "B")
term3 :: Term
term3 = Var (VarName "C")

testPretty :: String
testPretty = pretty (Var (VarName "Z"))

forbidden :: [VarName]
forbidden = [VarName "A", VarName "C"]

test :: Subst
test = replace (allVars rule1) (validVars forbidden rule1)