module KB (kb) where

import Util
import TRS
import Reduce
import CP
import Debug.Trace


orientation :: (Term -> Term -> Bool) -> (RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
{- orientation -}

composition :: (Rule, RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
{- composition -}

deduction   :: (Rule, RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
{- deduction   -}
      
collapse    :: (Rule, RuleSet, EquationSet) -> (Rule, RuleSet, EquationSet)
{- collapse    -}

simplification :: (RuleSet, EquationSet) -> (RuleSet, EquationSet)
{- simplification -}

deletion       :: (RuleSet, EquationSet) -> (RuleSet, EquationSet)
{- deletion    -}

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
