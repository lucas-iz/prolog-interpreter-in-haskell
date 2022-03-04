{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe3
import Test.QuickCheck
import Control.Monad

-- 1. Definieren Sie einen Datentyp Subst zur Repräsentation von Substitutionen.
-- {A->B} // (a,b)
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
apply :: Subst -> Term -> Term
apply (Subst []) t = t
apply (Subst [(a,b)]) (Var t) | a == t    = b
                              | otherwise = Var t
apply (Subst (r:rs)) (Var t)  = apply (Subst rs) (apply (Subst [r]) (Var t))
apply (Subst [(a,b)]) (Comb n list) = Comb n (map sub list)
   where
      sub t = apply (Subst [(a,b)]) t
apply (Subst (r:rs)) (Comb n list) = apply (Subst rs) (apply (Subst [r]) (Comb n list))

-- 5. 
compose :: Subst -> Subst -> Subst
compose (Subst []) (Subst []) = empty
compose (Subst []) s          = s
compose s          (Subst []) = s
compose (Subst [(a,b)]) (Subst [(c,d)]) = Subst [(c,apply (Subst [(a,b)]) d)]

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

-- Testsubstitution für 7.
test3 :: Subst
test3 = restrictTo (Subst [(VarName "D", Var (VarName "E")) , (VarName "F", Var (VarName "G")), (VarName "H", Var (VarName "I"))]) [VarName "D", VarName "F", VarName "H"]

-- 7. pretty (!! Bekommen Pretty nicht aus Aufgabe 2 exportiert, daher copy and paste für class Pretty und der Pretty Instanz für Term) 
class Pretty a where
  pretty :: a -> String

instance Pretty Subst where
  pretty (Subst []) = "{}"
  pretty (Subst [(VarName a,b)]) | domain(Subst [(VarName a,b)]) == [] = "{}"
                                 | otherwise = "{" ++ a ++ " -> " ++ pretty b ++ "}"                                 
  pretty (Subst ((VarName a,b) : cs)) = "{" ++ a ++ " -> " ++ pretty b ++  subPretty (Subst cs) ++ "}"
   where
     subPretty (Subst []) = ""
     subPretty (Subst ((VarName x,y) : zs)) = ", " ++ x ++ " -> " ++ pretty y ++ subPretty (Subst zs)

instance Pretty Term where
  pretty (Var (VarName x)) = x
  pretty (Comb x []) = x
  pretty (Comb x xs) = x ++ "(" ++ subPretty xs ++ ")"
    where
      subPretty [y] = pretty y
      subPretty (y : ys) = pretty y ++ ", " ++ subPretty ys
      subPretty [] = []

--Tests für Pretty
test :: String
test = pretty (single (VarName "A") (Var (VarName "B")))
test1 :: String
test1 = pretty (Subst [(VarName "D", Var (VarName "E")) , (VarName "F", Var (VarName "G")), (VarName "H", Var (VarName "I"))])
test2 :: String
test2 = pretty (single (VarName "F") (Comb "f" [Var (VarName "D"), Comb "true" []]))


-- 8. Instanz für Vars / allVars
instance Vars Subst where
   allVars (Subst []) = []
   --allVars (Subst ((a,b):xs)) = [a] ++ allVars b ++ allVars (Subst xs)
   allVars (Subst ((a,b):xs))
                     | Var a == b = allVars (Subst xs)
                     | otherwise = [a] ++ allVars b ++ allVars (Subst xs)

-- 9. Instanz für das Testen der Eigenschaften
instance Arbitrary Subst where
   arbitrary = do
      arity <- choose (0, 2)
      frequency [ (2, Subst <$> replicateM arity arbitrary) ]

-- 10. Funktionen zum Testen der Eigenschaften
prop_applyEmpty :: Term -> Bool
prop_applyEmpty t = apply empty t == t

prop_applySingle :: VarName -> Term -> Bool
prop_applySingle x t = apply (single x t) (Var x) == t

-- prop_applyCompose

prop_domainEmpty :: Bool
prop_domainEmpty = domain empty == []

prop_domainSingle :: VarName -> Bool
prop_domainSingle x = domain (single x (Var x)) == []

prop_domainSingle2 :: VarName -> Term -> Property
prop_domainSingle2 x t = t /= Var x ==> domain (single x t) == [x]

-- prop_domainCompose

-- prop_domainCompose2

prop_allVarsEmpty :: Bool
prop_allVarsEmpty = allVars empty == []

prop_allVarsSingle :: VarName -> Bool
prop_allVarsSingle x = allVars (single x (Var x)) == []
 
prop_allVarsSingle2 :: VarName -> Term -> Property
prop_allVarsSingle2 x t = t /= Var x ==> allVars (single x t) == allVars t ++ [x] 
                                      || allVars (single x t) == [x] ++ allVars t

-- prop_allVarsCompose

-- prop_allVarsCompose2

prop_domainAllVars :: Subst -> Bool
prop_domainAllVars s = testF s
                      
-- Testfunktion für prop_domainAllVars
testF :: Subst -> Bool
testF (Subst []) = True
testF (Subst s) = hilfF (domain (Subst s))
  where
    hilfF [] = True
    hilfF (d : ds) = d `elem` allVars (Subst s) && hilfF ds

prop_restrictEmpty :: [VarName] -> Bool 
prop_restrictEmpty xs = domain (restrictTo empty xs) == []

prop_restrictTo :: Subst -> [VarName] -> Bool 
prop_restrictTo s xs = subProp (restrictTo s xs) xs 

-- Testfunktion für prop_restrictTo
subProp :: Subst -> [VarName] -> Bool 
subProp (Subst []) _ = True
subProp (Subst s) xs = helfer (domain (Subst s))
   where
      helfer [] = True 
      helfer (d:ds) = d `elem` xs && helfer ds