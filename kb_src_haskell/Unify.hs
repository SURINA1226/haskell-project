module Unify where

import Util
import TRS
import Reduce


usub :: Subst -> Term -> Term -> (Bool, Subst)
usub alist (Node(VSym f1,ts1)) (Node(VSym f2,ts2)) =
    if f1==f2 then (True,alist)
    else  (True,(f1,Node(VSym f2,ts2)):fff alist [(f1,Node(VSym f2,ts2))])
usub alist (Node(VSym f1,ts1)) (Node(FSym f2,ts2)) =  
    if elem f1 (varlist (Node (FSym f2,ts2))) then
        (False,[])
    else
        --(False,[])
        (True,(f1,Node(FSym f2,ts2)):fff alist [(f1,Node(FSym f2,ts2))])
            
usub alist (Node(FSym f1,ts1)) (Node(VSym f2,ts2))=
    if elem f2 (varlist (Node (FSym f1, ts1))) then
        (False, [])
    else
        (True, (f2,Node(FSym f1,ts1)):fff alist [(f2,Node(FSym f1,ts1))])

usub alist (Node(FSym f1,ts1)) (Node(FSym f2,ts2))=
    if f1 == f2 then
        usublist alist ts1 ts2
    else
        (False,[])
--((String, Int), Term)
usublist :: Subst -> [Term] -> [Term] -> (Bool, Subst)
usublist alist [] []= (True,alist)
usublist alist [] x =(False,[])
usublist alist x [] =(False,[])
usublist alist (x:xs) (y:ys)=
        case usub alist x y of
            (True,alist1)-> usublist alist1 (substlist alist1 xs) (substlist alist1 ys)
            (False,_)->(False,[])

   
unify :: Term -> Term -> (Bool, Subst)
unify t1 t2 = usub [] t1 t2

fff :: Subst -> Subst -> Subst
fff [] _ =[]
fff ((x,y):xs) t = (x,subst t y):fff xs t 
