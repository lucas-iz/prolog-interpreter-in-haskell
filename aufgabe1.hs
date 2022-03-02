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

instance Pretty Rule where
  pretty (Rule t []) = pretty t ++ "."
  pretty (Rule t ts) = pretty t ++ " :- " ++ subPretty ts ++ "."
    where
      subPretty [y] = pretty y
      subPretty (y : ys) = pretty y ++ ", " ++ subPretty ys
      subPretty [] = []

instance Pretty Prog where
  pretty (Prog []) = ""
  pretty (Prog [p]) = pretty p
  pretty (Prog (p : ps)) = pretty p ++ "\n" ++ pretty (Prog ps)

instance Pretty Goal where
  pretty (Goal []) = "?- ."
  pretty (Goal [g]) = "?- " ++ pretty g ++ "."
  pretty (Goal (g : gs)) = "?- " ++ subPretty (g : gs)
    where
      subPretty (l : ls) = pretty l ++ ", " ++ pretty (Goal ls)
      subPretty [] = []

-- TODO => Goal-Instanzen
-- ghci > pretty (Goal [])
-- "?- ."
-- ghci > pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []]])
-- "?- =(X, false)."
-- ghci > pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []], Comb "=" [Var (VarName "X"), Comb "true" []]])
-- "?- =(X, false), =(X, true)."