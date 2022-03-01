import Type

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Var (VarName x)) = x
  pretty (Comb x []) = x
  pretty (Comb x xs) = x ++ "(" ++ subPretty xs ++ ")"
    where
      subPretty [y] = pretty y
      subPretty (y : ys) = pretty y ++ ", " ++ subPretty ys
      subPretty [] = []

-- TODO => Rules-Instanzen
-- ghci > pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [])
-- "f(X, true)."
-- ghci > pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")]])
-- "f(X, true) :- g(X)."
-- ghci > pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")], Comb "h" []])
-- "f(X, true) :- g(X), h."

-- TODO => Prog-Instanzen
-- ghci > pretty (Prog [])
-- ""
-- ghci > pretty (Prog [Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) []])
-- "f(X, true)."
-- ghci > pretty (Prog [Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]])
-- "append([], Ys, Ys).\nappend(.(X, Xs), Ys, .(X, Zs)) :- append(Xs, Ys, Zs)."

-- TODO => Goal-Instanzen
-- ghci > pretty (Goal [])
-- "?- ."
-- ghci > pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []]])
-- "?- =(X, false)."
-- ghci > pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []], Comb "=" [Var (VarName "X"), Comb "true" []]])
-- "?- =(X, false), =(X, true)."