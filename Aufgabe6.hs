{-# LANGUAGE TemplateHaskell #-}
import Type
-- import Aufgabe2
import Aufgabe3
import Aufgabe4 
-- import Aufgabe5
import Data.List
import Test.QuickCheck

-- Ändert die Namen aller Variablen in einer Regel.
rename :: [VarName] -> Rule -> Rule
rename vars (Rule t []) = Rule (apply (replace (allVars (Rule t [])) (validVars vars (Rule t []))) t) []
rename vars (Rule t ts) = Rule (apply (replace (allVars (Rule t ts)) (validVars vars (Rule t ts))) t)
                               (map (apply (replace (allVars (Rule t ts)) (validVars vars (Rule t ts)))) ts)

-- Liste aller erlaubten Variablen.
-- (Alle freshVars, die nicht in der Liste oder der Rule enthalten sind)
validVars :: [VarName] -> Rule -> [VarName]
validVars vars rule = freshVars \\ (vars ++ allVars rule)

-- Liefert eine Substitution von nicht erlaubten zu erlaubten Variablen.
-- 1. Argument: Liste aller Variablen einer Rule (nicht erlaubt)
-- 2. Argument: Liste aller erlaubten Variablen
replace :: [VarName] -> [VarName] -> Subst
replace [] _ = empty
replace _ [] = empty
replace t v = Subst (map (\(a,b) -> (a, Var b)) (zip t v))

-- Prüft ob der Schnitt zweier Mengen leer ist. True falls ja. 
-- (Wird zum Testen der Eigenschaften verwendet)
leererSchnitt :: [VarName] -> [VarName] -> Bool
leererSchnitt [] _ = True 
leererSchnitt _ [] = True
leererSchnitt (x:xs) ys = x `notElem` ys && leererSchnitt xs ys

-- Testfunktionen für alle gegebenen Eigenschaften.
prop_allVarsX2 :: [VarName] -> Rule -> Bool 
prop_allVarsX2 xs r = allVars (rename xs r) `leererSchnitt` allVars r

prop_allVarsX1 :: [VarName] -> Rule -> Bool 
prop_allVarsX1 xs r = allVars (rename xs r) `leererSchnitt` xs

prop_varsRename :: [VarName] -> Rule -> Bool 
prop_varsRename xs r = VarName "_" `notElem` allVars (rename xs r)

prop_varsRename2 :: [VarName] -> Rule -> Property 
prop_varsRename2 xs r = VarName "_" `notElem` allVars r ==> length (allVars (rename xs r)) == length (allVars r)

prop_varsRename3 :: [VarName] -> Rule -> Bool 
prop_varsRename3 xs r = length (allVars (rename xs r)) >= length (allVars r)

-- For testing all tests.
return []
runTests3 :: IO Bool 
runTests3 = $quickCheckAll