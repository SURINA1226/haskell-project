module Reduce where

import Util
import TRS
import Data.Binary.Get (Decoder(Fail))

--subst定義
subst :: Subst -> Term -> Term
subst  t (Node(VSym x,_))= 
      case find x t of
            Nothing -> Node(VSym x,[])
            Just v  -> v
subst t (Node(FSym x ,xs))=Node(FSym x,substlist t xs)

substlist :: Subst -> [Term] -> [Term]
substlist t [] = []
substlist t (y:ys) = [subst t y] ++ substlist t ys 

--match定義
match :: Pattern -> Term -> (Bool, Subst)
match  (Node(VSym x,y)) t = (True,[(x,t)])
match  (Node(FSym _,_)) (Node(VSym _,_)) = (False,[])

match (Node(FSym x,xs)) (Node(FSym y,ys)) = 
      --if (Node(FSym x,xs))== (Node(FSym y,ys))then(True,(x,y))
      if x == y then 
           matchlist [] xs ys
      else (False,[])

matchlist :: Subst -> [Pattern] -> [Term] -> (Bool, Subst)
matchlist t  [] []=(True,t)
matchlist t  x []=(True,t)
matchlist t (x:xs) (y:ys)=
    case match x y of
      (False, []) -> (False, [])
      (True, theta) ->
          case app t theta of
            Nothing -> (False, [])
            Just sigma -> matchlist sigma xs ys

app :: (Eq k, Eq a) => Assoc k a -> Assoc k a -> Maybe (Assoc k a)
app [] ys = Just ys
app ((x,v):xs) ys =
        case find x ys of
            Nothing -> app xs ((x,v):ys)
            Just v1 -> 
                  if v==v1 then app xs ys
                  else  Nothing  


--rewerite定義
rewrite :: RuleSet -> Term -> (Bool, Term)
rewrite [] t =(False,t)
rewrite ((r1,r2):rs) t =
    case match r1 t of
      (True, theta) -> (True, subst theta r2)
      (False, _) -> rewrite rs t
{-
linf :: RuleSet -> Term -> Term
linf rs (Node(x,y))=  
      let t = map (linf rs) y in
             t `seq` case rewrite rs (Node (x, t)) of
                     (True, tt) -> linf rs tt
                     (False, f) -> f -}
{-
linf :: RuleSet -> Term -> Term
linf rs (Node(x,y))=  
       case linflist rs y of
            t ->
                  case rewrite rs (Node(x,t)) of
                  (True,tt) -> linf rs tt
                  (False,f) -> f  
  --          
  --       let t = map (linf rs) y in    
            
      
linflist :: RuleSet -> [Term] -> [Term]
linflist rs [] =[]
linflist rs (y:ys)=linf rs y :linflist rs ys
-}

linf :: RuleSet -> Term -> Term
linf rs (Node(x,y))= 
       case set rs [] of
            k-> 
                  if check (Node(x,y)) rs k then
                        case lisubst rs (Node(x,y)) k of
                               m ->linftop rs m k
                  else Node(x,lisubstlist rs y k)
     
      

linftop:: RuleSet -> Term ->[String]-> Term
linftop rs t k=     
            if check t rs k then
                  case rewrite rs t of
                        (True,tt)-> lisubst rs tt k
                        (False,ff)->t
            else t
      
lisubst:: RuleSet -> Term->[String] -> Term
lisubst t (Node(VSym x,y)) k= 
      if check (Node(VSym x,y)) t k then
            snd (rewrite t (Node(VSym x,y)))
       else (Node(VSym x,y))
lisubst t (Node(FSym x,y)) k=
      if check (Node(FSym x,y)) t k then
            case lisubstlist t y k of
                   m->linftop t (Node(FSym x,m)) k
      else (Node(FSym x,lisubstlist t y k))
--($!) :: (a -> b) -> a-> (a -> b)->[a] -> b
--f  $! f1 (x:xs) = (f x)  `seq` f x
lisubstlist:: RuleSet -> [Term] -> [String]-> [Term]
lisubstlist rs [] _ =[]
lisubstlist rs (Node(VSym x,y):xs) k=
      if check (Node(VSym x,y)) rs k then
            lisubst rs (Node(VSym x,y)) k :lisubstlist rs xs k
      else (Node(VSym x,lisubstlist rs y k)):lisubstlist rs xs k
lisubstlist rs (Node(FSym x,y):xs) k=
      if check (Node(FSym x,y)) rs k then
            lisubst rs  (Node(FSym x,y)) k:lisubstlist rs xs k
      else  (Node(FSym x,lisubstlist rs y k)):lisubstlist rs xs k

--判断
check ::Term ->RuleSet->[String]->Bool
check (Node(VSym x,y)) t k=False
check (Node(FSym x,y)) t k=checklist (Node(FSym x,y)) k
                         
checklist ::Term->[String]->Bool
checklist x [] = False
--checklist (Node(FSym x,y)) (Node(VSym x1,y1):xs) =False
checklist (Node(FSym x,y)) (p:ps) =
      (x == p) || checklist (Node (FSym x, y)) ps

set ::RuleSet->[String]->[String]
set [] p=p
set ((Node(VSym x,y),n):ts) p=set ts p
set ((Node(FSym x,y),n):ts) p=
      if elem (prsym (FSym x) )  p then set ts p
      else     set ts (prsym (FSym x):p)

--postep定義
postep :: RuleSet -> Term -> (Bool, Term)
postep rs (Node(x,y)) =
      case rewrite rs (Node(x,y)) of
            (True,t) -> (True,t)
            (False,f) ->
                  case  posteplist rs y of
                        (True,t) ->(True,Node(x,t))
                        (False,ff) ->(False,f) 
                                                           
--posteplist定義                       
posteplist :: RuleSet -> [Term]-> (Bool,[Term])
posteplist rs []=(False,[])
posteplist rs (y:ys)= 
      case postep rs y of
            (True,t)-> (True,t:snd(posteplist rs ys))
            (False,f)->
                  case posteplist rs ys of
                        (p,q) ->(p,f:q)
--ponf定義                 
ponf :: RuleSet -> Term -> Term
ponf rs t = 
      case postep rs t of
            (True,tt) -> ponf rs tt
            (False,ff) -> ff
--lostep定義       
lostep :: RuleSet -> Term -> (Bool, Term)
lostep rs (Node(x,y)) =
      case rewrite rs  (Node(x,y)) of
            (True,t) -> (True,t)
            (False,f) ->
                  case  losteplist rs y of
                        (True,tt) ->(True,Node(x,tt))
                        (False,_) ->(False,f)
--losteplist定義
losteplist :: RuleSet -> [Term] -> (Bool, [Term])                        
losteplist rs []=(False,[])
losteplist rs (y:ys)= 
      case lostep rs y of
            (True,t) -> (True,t:ys)
            (False,f) -> 
                  case losteplist rs ys of
                        (p,q) ->(p,f:q)
--lonf定義
lonf :: RuleSet -> Term -> Term
lonf rs t =
      case lostep rs t of
            (True,tt) -> lonf rs tt
            (False,ff) -> ff

 
