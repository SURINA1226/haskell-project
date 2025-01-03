module KB(kb)  where

import Util
import TRS
   -- ( preqs, prrules, rdeq, rdrule, EquationSet, Rule, RuleSet, Term, Symbol (VSym) )
import Reduce
import CP
import Debug.Trace
import Order

orientation :: (Term -> Term -> Bool) -> (RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
{-orientation funct (x,(y1,y2):ys)=
  if (funct y1 y2) then ((y1,y2),x,ys)
  else if funct y2 y1 then ((y2,y1),x,ys)
  else 
    case orientation funct (x,ys) of
      (a,b,c)->(a,b,(y1,y2):c)-}
    
 
orientation funct ([],(y1,y2):ys)= 
   case select4 (y1,y2)  ((y1,y2):ys)   of
      (t1,t2)-> 
          if (funct t1 t2) then orientation funct ([(t1,t2)],ys)
          else if (funct t2 t1) then orientation funct ([(t2,t1)],ys)
          else 
            case orientation funct ([],delete (t1,t2) ((y1,y2):ys)) of
              (a,b,c)->(a,b,(t1,t2):c)


orientation funct (x,(y1,y2):ys)=
    case select4 (y1,y2)  ((y1,y2):ys)   of
      (t1,t2)->
        if (funct t1 t2) then ((t1,t2),x,delete (t1,t2) ((y1,y2):ys))
        else if  funct t2 t1 then ((t2,t1),x,delete (t1,t2) ((y1,y2):ys))
        else  
          case orientation funct (x,delete (t1,t2) ((y1,y2):ys)) of
             (a,b,c)->(a,b,(t1,t2):c)




select4 ::Equation ->EquationSet->Equation
select4 (m,n)  [] =(m,n)
select4 (m,n)  ((y1,y2):ys)= 
 
  if tsize m > tsize n then 
    if tsize y1 > tsize y2 then 
      if tsize m >tsize y1 then select4 (y1,y2)  ys
     
        
      else select4 (m,n)  ys
    else
      if tsize m > tsize y2 then select4 (y1,y2)  ys
      else select4 (m,n)  ys 
  else 
    if tsize y1 > tsize y2 then 
      if tsize n > tsize y1 then select4 (y1,y2)  ys
      else select4 (m,n)  ys
    else
      if tsize n > tsize y2 then select4 (y1,y2)  ys
      else select4 (m,n)  ys 




delete ::Equation->EquationSet->EquationSet
delete p (t:ts)=
  if p==t then ts
  else (t:delete p ts)
--Composition
composition :: (Rule, RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
composition (r1,m,e) =(r1,comlist r1 m,e)

comlist :: Rule->RuleSet->RuleSet
comlist r1 [] =[]
comlist r1 ((a,b):rs) = (a,ponf (r1:(a,b):rs) b) :comlist r1 rs

--Deduction
deduction   :: (Rule, RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
deduction (r,t,e)=(r,t,e++deductionlist r t )  
 
deductionlist ::Rule->RuleSet->EquationSet
deductionlist r1 [] =tail(cpair1 r1 r1)
deductionlist r1 (r2:rs) =   deductionlist r1 rs ++cpair r1 r2 
      
--Collapse
collapse    :: (Rule, RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
collapse   (r,t,e)=
  case collapselist (r,t,e) of
    (a,b)-> (r,a,b)
collapselist ::(Rule,RuleSet,RuleSet)->(RuleSet,RuleSet)
collapselist (r,[],e)=([],e)
collapselist   ((m,n), (t1,t2):ts, e)= 
  case ponf [(m,n)] t1 of 
    (x)->
      if x==t1 then 
        case collapselist  ((m,n), ts, e ) of
          (f,l)->((t1,t2):f, l)
          
      else 
        case collapselist  ((m,n), ts, e++[(x,t2)]) of
          (f,l)->(f,l)


--Simplification
simplification :: (RuleSet, EquationSet) -> (RuleSet, EquationSet)
simplification (r,e)=(r,simplificationlist r e)
simplificationlist ::RuleSet->EquationSet->EquationSet
simplificationlist r []=[]
simplificationlist r ((e1,e2):es) = (ponf r e1,ponf r e2):simplificationlist r es

--Deletion
deletion    :: (RuleSet, EquationSet) -> (RuleSet, EquationSet)
deletion (r,e) = (r,deletionlist e)  
deletionlist :: EquationSet->EquationSet
deletionlist [] =[] 

deletionlist ((e1,e2):es) = 
  if e1==e2 then deletionlist es
  else (e1,e2):deletionlist es


kbsub :: (Term -> Term -> Bool) -> (RuleSet, EquationSet) -> Int -> RuleSet
kbsub _ (rs, []) n = trace ("Success ("++(show n)++" steps).\n") rs
kbsub grter (rs, es) n = 
  trace 
    ("=== "++(show (n+1))++" step ===\n"
     ++"R =\n"++(prrules rs1)
     ++"E =\n"++(preqs es1))
    (kbsub grter (rs1, es1) (n+1))
  where (rs1, es1) = kbstep grter (rs, es)
kbstep :: (Term -> Term -> Bool) -> (RuleSet, EquationSet) -> (RuleSet, EquationSet)
kbstep grter (rs, es) =
    (deletion
      (simplification
        ((\(r, rs, es) -> (r:rs, es))
          (collapse
            (deduction
              (composition (orientation grter (rs, es))))))))

kb :: (Term -> Term -> Bool) -> EquationSet -> RuleSet
kb grter es =
  kbsub grter ([], filter (\(t1,t2) -> t1 /= t2) es ) 0
