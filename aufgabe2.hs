module Aufgabe2 where
import Type
import Data.List

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Var (VarName x)) = x
  pretty (Comb x []) = x
  pretty (Comb x xs) = x ++ "(" ++ intercalate "," (map pretty xs) ++ ")"

instance Pretty Rule where
  pretty (Rule t []) = pretty t ++ "."
  pretty (Rule t ts) = pretty t ++ " :- " ++ intercalate "," (map pretty ts) ++ "."

instance Pretty Prog where
  pretty (Prog []) = ""
  pretty (Prog [p]) = pretty p
  pretty (Prog (p : ps)) = pretty p ++ "\n" ++ pretty (Prog ps)

instance Pretty Goal where
  pretty (Goal []) = "?- ."
  pretty (Goal [g]) = "?- " ++ pretty g ++ "."
  pretty (Goal gs) = "?- " ++ intercalate "," (map pretty gs) ++ "."


-- Test-Möglichkeiten. ---
-- term :: Term
-- term = Comb "f" [Var (VarName "A"), Comb "true" []]
-- term2 :: Term
-- term2 = Comb "h" [Comb "true" []]
-- term3 :: Term
-- term3 = Var (VarName "Z")

-- regel :: Rule
-- regel = Rule term [term2, term3]
-- regel2 :: Rule
-- regel2 = Rule term3 [term2, term]

-- programm :: Prog
-- programm = Prog [regel, regel2]

-- goal :: Goal
-- goal = Goal [term, term2, term3]