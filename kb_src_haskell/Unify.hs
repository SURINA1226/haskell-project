module Unify where

import Util
import TRS
import Reduce

{-
usub :: Subst -> Term -> Term -> (Bool, Subst)
usub .....
usublist :: Subst -> [Term] -> [Term] -> (Bool, Subst)
usublist .....
-}

--unify :: Term -> Term -> (Bool, Subst)
--unify t1 t2 = usub [] t1 t2
