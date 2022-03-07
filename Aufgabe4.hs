{-# LANGUAGE TemplateHaskell #-}
module Aufgabe4 where
import Type
import Aufgabe2
import Aufgabe3
import Test.QuickCheck
import Control.Monad

-- 1. Definieren Sie einen Datentyp Subst zur Repräsentation von Substitutionen.
-- {A->B} // [(a,b)]
data Subst = Subst [(VarName, Term)]
  deriving Show

-- 2. Definieren Sie eine Funktion domain :: Subst -> [VarName], die den Definitionsbereich einer Substitution zurückgibt. 
-- Mit dem Definitionsbereich einer Substitution sind dabei aus praktischen Gründen nur diejenigen Variablen gemeint, die nicht auf sich selbst abgebildet werden.
-- Aufruf: domain(Subst (VarName "c", Comb "1" [Var (VarName "b"), Var (VarName "c")]))
domain :: Subst -> [VarName]
domain (Subst []) = []
domain (Subst ((a,b) : rest))
                     | Var a == b = domain (Subst rest)     -- verhindert {A->A}
                     | otherwise = a : domain (Subst rest)

-- 3. Definieren Sie dann zwei Funktionen empty :: Subst und single :: VarName -> Term -> Subst zum Erstellen einer leeren Substitution bzw. einer Substitution,
-- die lediglich eine einzelne Variable auf einen Term abbildet.
empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single a b = Subst [(a,b)]

-- 4. Implementieren Sie eine Funktion apply :: Subst -> Term -> Term, die eine Substitution auf einen Term anwendet.
-- Complete change (Es wird nun gleichzeitig substituiert (A -> B, B -> C ergibt nicht mehr A -> C))
apply :: Subst -> Term -> Term
apply (Subst []) t = t
apply (Subst ((a,b) : cs)) (Var t) | a == t = b
                                   | otherwise = apply (Subst cs) (Var t)
apply (Subst s) (Comb f t) = Comb f (map (apply (Subst s)) t)

-- 5. 
compose :: Subst -> Subst -> Subst
compose (Subst []) (Subst []) = empty
compose (Subst []) s          = s
compose s          (Subst []) = s
compose (Subst (a:as)) (Subst (b:bs)) = Subst (map (\(r,s) -> (r, apply (Subst (a:as)) s)) (b:bs) ++ filter (\(x,_) -> x `notElem` domain (Subst (b:bs))) (a:as))

-- 6. Implementieren Sie weiterhin eine Funktion restrictTo :: Subst -> [VarName] -> Subst,
-- die eine Substitution bzw. deren Definitionsbereich auf eine gegebene Variablenmenge einschränkt.
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst []) _ = Subst []
restrictTo _ [] = empty
restrictTo (Subst s) v = Subst (sub s v)
  where
    sub [] _ = []
    sub (x:xs) t | hf (Subst [x]) t = x : sub xs t
                 | otherwise = sub xs t

-- Hilfsfunktion: überprüft, ob alle Variablen in der Domäne einer Substitution in einer gegebenen Liste an Variablennamen vorkommen
hf :: Subst -> [VarName] -> Bool
hf (Subst []) _ = True
hf _ [] = False
hf (Subst (s:_)) v =  hhf (domain (Subst [s])) v
  where
    hhf [] _ = True
    hhf (d:dd) t = d `elem` t && hhf dd t

-- 7. pretty 
instance Pretty Subst where
  pretty (Subst []) = "{}"
  pretty (Subst [(VarName a,b)]) | domain(Subst [(VarName a,b)]) == [] = "{}"
                                 | otherwise = "{" ++ a ++ " -> " ++ pretty b ++ "}"
  pretty (Subst s) = "{" ++ subPretty (filter (\(x,y) -> Var x /= y) s) ++ "}"
   where
      subPretty :: [(VarName, Term)] -> String
      subPretty [] = ""
      subPretty [(VarName a,b)]    = a ++ " -> " ++ pretty b
      subPretty ((VarName a,b):cs) = a ++ " -> " ++ pretty b ++ ", " ++ subPretty cs

-- 8. Instanz für Vars / allVars
instance Vars Subst where
   allVars (Subst []) = []
   allVars (Subst ((a,b):xs))
                     | Var a == b = allVars (Subst xs)
                     | otherwise = [a] ++ allVars b ++ allVars (Subst xs)

-- 9. Instanz für das Testen der Eigenschaften
instance Arbitrary Subst where
   arbitrary = do
      arity <- choose (0,4)
      Subst <$> (replicateM arity arbitrary `suchThat` hasNoDuplicates)

-- Returns True if the given list has no duplicate VarName.
hasNoDuplicates :: [(VarName, Term)] -> Bool
hasNoDuplicates [] = True 
hasNoDuplicates xs = sub xs []
   where
        sub [] _               = True
        sub ((a,_):cs) visited | a `elem` visited = False
                               | otherwise        = sub cs (a:visited)

-- 10. Funktionen zum Testen der Eigenschaften
teilmenge :: [VarName] -> [VarName] -> Bool 
teilmenge [] _  = True 
teilmenge _ []  = False 
teilmenge (x:xs) ys = x `elem` ys && teilmenge xs ys

prop_applyEmpty :: Term -> Bool
prop_applyEmpty t = apply empty t == t

prop_applySingle :: VarName -> Term -> Bool
prop_applySingle x t = apply (single x t) (Var x) == t

prop_applyCompose :: Subst -> Subst -> Term -> Bool 
prop_applyCompose s1 s2 t = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_domainEmpty :: Bool
prop_domainEmpty = domain empty == []

prop_domainSingle :: VarName -> Bool
prop_domainSingle x = domain (single x (Var x)) == []

prop_domainSingle2 :: VarName -> Term -> Property
prop_domainSingle2 x t = t /= Var x ==> domain (single x t) == [x]

prop_domainCompose :: Subst -> Subst -> Bool 
prop_domainCompose s1 s2 = domain(compose s1 s2) `teilmenge` (domain s1 ++ domain s2)

prop_domainCompose2 :: VarName -> VarName -> Property
prop_domainCompose2 x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]

prop_allVarsEmpty :: Bool
prop_allVarsEmpty = allVars empty == []

prop_allVarsSingle :: VarName -> Bool
prop_allVarsSingle x = allVars (single x (Var x)) == []

prop_allVarsSingle2 :: VarName -> Term -> Property
prop_allVarsSingle2 x t = t /= Var x ==> allVars (single x t) == allVars t ++ [x]
                                      || allVars (single x t) == [x] ++ allVars t

prop_allVarsCompose :: Subst -> Subst -> Bool
prop_allVarsCompose s1 s2 = allVars (compose s1 s2) `teilmenge` (allVars s1 ++ allVars s2)

prop_allVarsCompose2 :: VarName -> VarName -> Property
prop_allVarsCompose2 x1 x2 = x1 /= x2 ==> allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x1,x2] || 
                                          allVars (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2,x1]

prop_domainAllVars :: Subst -> Bool
prop_domainAllVars s = (domain s) `teilmenge` (allVars s)

prop_restrictEmpty :: [VarName] -> Bool
prop_restrictEmpty xs = domain (restrictTo empty xs) == []

prop_restrictTo :: Subst -> [VarName] -> Bool
prop_restrictTo s xs = domain (restrictTo s xs) `teilmenge` xs

-- For testing all tests.
return []
runTests :: IO Bool
runTests = $quickCheckAll