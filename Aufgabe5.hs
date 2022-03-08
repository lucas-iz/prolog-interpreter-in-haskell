{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe2
import Aufgabe3
import Aufgabe4


ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 | t1 == t2 = Nothing  -- 1.

ds (Var a) t = Just (Var a, t)         -- 2.          
ds t (Var a) = Just (t, Var a)         -- 2.        

ds (Comb f a) (Comb g b) | f /= g || length a /= length b = Just (Comb f a, Comb g b)  -- 3.1
                         | otherwise = ds (fst (head (filter (\(x,y) -> x /= y) (zip a b))))
                                          (snd (head (filter (\(x,y) -> x /= y) (zip a b))))


unify :: Term -> Term -> Maybe Subst
unify t1 t2 = sub t1 t2 (Subst []) 

sub :: Term -> Term -> Subst -> Maybe Subst
sub t1 t2 s | ds (apply s t1) (apply s t2) == Nothing = Just s
            | otherwise = sub2 (ds (apply s t1) (apply s t2)) t1 t2 s
            
sub2 :: Maybe (Term, Term) -> Term -> Term -> Subst -> Maybe Subst
sub2 Nothing _ _ _ = Nothing -- ???
sub2 (Just (Var a,b)) t1 t2 s | a `notElem` allVars b = sub t1 t2 (combine s (Subst [(a,b)]))
                              | otherwise = Nothing
sub2 (Just (a, Var b)) t1 t2 s | b `notElem` allVars a = sub t1 t2 (combine s (Subst [(b,a)]))
                               | otherwise = Nothing
sub2 (Just (_,_)) _ _ _ = Nothing

combine :: Subst -> Subst -> Subst 
combine (Subst []) s = s
combine s (Subst []) = s
combine (Subst s1) (Subst s2) = Subst (s1++s2)

term1 :: Term
term1 = Comb "ehemann" [Comb "monika" [], Var(VarName "M")]
term2 :: Term
term2 = Comb "ehemann" [Var(VarName "F"),Comb "herbert" []]

-- -- Testfälle      
-- t1 :: Term
-- t1 = Comb "f" [
--       Var (VarName "A"), 
--       Comb "g" [
--          Comb "s" [
--             Var (VarName "U")
--          ],
--          Comb "t" [
--             Var (VarName "V")
--          ]
--       ]
--    ]

-- t2 :: Term
-- t2 = Comb "f" [
--       Var (VarName "A"), 
--       Comb "g" [
--          Comb "s" [
--             Var (VarName "X")
--          ],
--          Comb "t" [
--             Var (VarName "Z")
--          ]
--       ]
--    ]

-- t3 :: Term 
-- t3 = Var(VarName "A")

-- t4 :: Term
-- t4 = Var (VarName "B") 

