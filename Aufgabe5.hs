{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Type
import Aufgabe3
import Aufgabe4


ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 | t1 == t2 = Nothing  -- 1.

ds (Var a) t | Var a == t = Nothing              -- 2.
             | Var a /= t = Just (Var a, t)

ds t (Var a) | Var a == t = Nothing              -- 2.
             | Var a /= t = Just (t, Var a)


ds (Comb f a) (Comb g b) | f /= g || length a /= length b = Just (Comb f a, Comb g b)  -- 3.1
                         | otherwise = ds 
                                (fst (head (filter (\(x,y) -> x /= y) (zip a b))))
                                (snd (head (filter (\(x,y) -> x /= y) (zip a b))))
                         








ds (Comb _ []) _ = Nothing                                  -- Fall 1: Der erste Term besitzt keine Variablen -> keine Unstimmigkeitsmenge
ds _ (Comb _ []) = Nothing                                  -- Fall 2: Der zweite Term besitzt keine Variablen -> keine Unstimmigkeitsmenge

ds (Var a) (Var b) | a == b = Nothing                       -- Fall 3: Beide Terme bestehen aus einer Variablen und die sind gleich -> keine Unstimmigkeitsmenge
                   | otherwise = Just (Var a,Var b)         -- Fall 4:                                          und die sind nicht gleich -> Unstimmigkeitsmenge (a,b)



         


