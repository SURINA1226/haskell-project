module Reduce where

import Util
import TRS

subst :: Subst -> Term ->  Term
subst  t (Node(VSym x,_))= 
      case find x t of
            Nothing -> Node(VSym x,[])
            Just v  -> v
subst t (Node(FSym x ,xs))=Node(FSym x,substlist t xs)

substlist :: Subst -> [Term] -> [Term]
substlist t [] = []
substlist t (y:ys) = [subst t y] ++ substlist t ys 


match :: Pattern -> Term -> (Bool, Subst)
match  (Node(VSym x,_)) t =  (True,[(x,t)])
match  (Node(FSym _,_)) (Node(VSym _,_)) = (False,[])
match (Node(FSym x,xs)) (Node(FSym y,ys)) = 
      if x == y then 
            matchlist [] xs ys  
      else (False,[])

matchlist :: Subst -> [Pattern] -> [Term] -> (Bool, Subst)
matchlist t [] [] = (True,t)
matchlist t (x:xs) (y:ys)=
      case match x y of
            (False,[]) -> (False,[])
            (True,theta) ->
                  case append t theta of
                        Nothing -> (False,[])
                        Just sigma -> matchlist sigma xs ys
                        
rewrite :: RuleSet -> Term -> (Bool, Term)
rewrite [] t =(False,t) 
rewrite ((r1,r2):rs) t = 
      case match r1 t of
            (True,theta) -> (True,subst theta r2)
            (False,_) -> rewrite rs t 


linf :: RuleSet -> Term -> Term
linf rs (Node(x,y))=  
       case linflist rs y of
            t ->
                  case rewrite rs (Node(x,t)) of
                  (True,tt) -> linf rs tt
                  (False,f) -> f  
            
      
linflist :: RuleSet -> [Term] -> [Term]
linflist rs [] =[]
linflist rs (y:ys)=linf rs y :linflist rs ys

postep :: RuleSet -> Term -> (Bool, Term)
postep rs (Node(x,y)) =
      case rewrite rs (Node(x,y)) of
            (True,t) -> (True,t)
            (False,f) ->
                  case  posteplist rs y of
                        (True,t) ->(True,Node(x,t))
                        (False,[]) ->(False,f)
posteplist :: RuleSet -> [Term] -> (Bool, [Term])
posteplist rs []=(False,[])
posteplist rs (y:ys)= 
      case rewrite rs y of
            (True,t) -> (True,t: snd(posteplist rs ys))
            (False,f) -> posteplist rs ys
ponf :: RuleSet -> Term -> Term
ponf rs t = 
      case postep rs t of
            (True,tt) -> ponf rs tt
            (False,ff) -> ff

lostep :: RuleSet -> Term -> (Bool, Term)
lostep rs (Node(x,y)) =
      case rewrite rs  (Node(x,y)) of
            (True,t) -> (True,t)
            (False,f) ->
                  case  losteplist rs y of
                        (True,t) ->(True,Node(x,t))
                        (False,[]) ->(False,f)
losteplist :: RuleSet -> [Term] -> (Bool, [Term])                        
losteplist rs []=(False,[])
losteplist rs (y:ys)= 
      case rewrite rs y of
            (True,t) -> (True,t:ys)
            (False,f) -> posteplist rs ys
lonf :: RuleSet -> Term -> Term
lonf rs t =
      case lostep rs t of
            (True,tt) -> lonf rs tt
            (False,ff) -> ff