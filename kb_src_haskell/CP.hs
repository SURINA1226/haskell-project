module CP where

import TRS
import Reduce
import Unify
import Rename

cp_component :: Term -> Rule -> [(Term, Subst)]
{-
cp_component ...
-}

cp_componentlist :: [Term] -> Rule -> [([Term], Subst)]
{-
cp_componentlist ...
-}

cp_assemble :: Term -> [(Term, Subst)] -> EquationSet
{-
cp_assemble ...
-}

cpair :: Rule -> Rule -> EquationSet
cpair r1 r2 = cp_assemble r3 (cp_component l3 (l4, r4)) ++ cp_assemble r4 (cp_component l4 (l3, r3))
  where ((l3, r3), (l4, r4)) = uniquevar (r1, r2)
