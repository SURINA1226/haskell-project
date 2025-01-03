module CP where

import TRS
import Reduce
import Unify
import Rename
    
cp_component :: Term -> Rule -> [(Term, Subst)]
cp_component (Node(VSym x,y)) (l2,r2) =[]
cp_component (Node(FSym x,y)) (l2,r2) =
  case unify (Node(FSym x,y)) l2 of
    (True,z) -> 
      (r2,z):cplist (cp_componentlist y (l2,r2)) (Node(FSym x,y))
    (False,[]) ->cplist (cp_componentlist y (l2,r2)) (Node(FSym x,y)) 


cplist :: [([Term], Subst)]->Term ->[(Term, Subst)]
cplist [] _  =[]
cplist ((t,b):ts) (Node(x,y))=(Node(x,t),b):cplist ts (Node(x,y))

cp_componentlist :: [Term] -> Rule -> [([Term], Subst)]
cp_componentlist [] _ =[]
cp_componentlist (x:xs) (l,r) = 
  cplistlist (cp_component x (l,r)) (x:xs)++ cplistlistlist (cp_componentlist xs (l,r)) (x:xs)

cplistlist :: [(Term, Subst)]->[Term]->[([Term], Subst)]
cplistlist [] _ =[]
cplistlist ((term,subst):xs) (x:xss) =
  (term:xss,subst):cplistlist xs (x:xss)
cplistlistlist ::[([Term], Subst)]->[Term] ->[([Term], Subst)]
cplistlistlist [] _=[]
cplistlistlist ((term,subst):xs) (x:xss)=
  (x:term,subst):cplistlistlist xs (x:xss)



cp_assemble :: Term -> [(Term, Subst)] -> EquationSet
cp_assemble _ [] =[]
cp_assemble l ((t,s):xs) =(subst s t,subst s l):cp_assemble l xs

cpair :: Rule -> Rule -> EquationSet
cpair r1 r2 = cp_assemble r3 (cp_component l3 (l4, r4)) ++cp_assemble r4 (cp_component l4 (l3, r3))
 where ((l3, r3), (l4, r4)) = uniquevar (r1, r2)


cpair1 :: Rule -> Rule -> EquationSet
cpair1 r1 r2 = cp_assemble r3 (cp_component l3 (l4, r4))
  where ((l3, r3), (l4, r4)) = uniquevar (r1, r2)
