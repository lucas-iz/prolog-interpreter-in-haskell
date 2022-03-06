import Type
import Aufgabe3
import Aufgabe4


ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 | t1 == t2 = Nothing  -- 1.

ds (Var a) t = Just (Var a, t)         -- 2.          
ds t (Var a) = Just (t, Var a)         -- 2.        

ds (Comb f a) (Comb g b) | f /= g || length a /= length b = Just (Comb f a, Comb g b)  -- 3.1
                         | otherwise = ds (fst (head (filter (\(x,y) -> x /= y) (zip a b))))
                                          (snd (head (filter (\(x,y) -> x /= y) (zip a b))))


-- unify :: Term -> Term -> Maybe Subst
-- unify t1 t2 | ds t1 t2 == Nothing = 




-- Testfälle      
t1 :: Term
t1 = Comb "f" [
      Var (VarName "A"), 
      Comb "g" [
         Comb "s" [
            Var (VarName "U")
         ],
         Comb "t" [
            Var (VarName "V")
         ]
      ]
   ]

t2 :: Term
t2 = Comb "f" [
      Var (VarName "A"), 
      Comb "g" [
         Comb "s" [
            Var (VarName "X")
         ],
         Comb "t" [
            Var (VarName "Z")
         ]
      ]
   ]

t3 :: Term 
t3 = Var(VarName "A")

t4 :: Term
t4 = Var (VarName "B") 

