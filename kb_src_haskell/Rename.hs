module Rename where

import Util
import TRS


vars (Node (VSym v, ts)) = union [v] (varslist ts)
vars (Node (_, ts)) = varslist ts
varslist [] = []
varslist (t:ts) = union (vars t) (varslist ts)

findMax [] = 0
findMax ((v,n):vs) = if n <= m then m else n
  where m = findMax vs


mkrename :: [(String, Int)] -> Int -> [(String, Int)] -> [((String, Int), (String, Int))]
mkrename _ _ [] = []
mkrename vs1 n (v2:vs2) =
  let
      mkvar (v,c) =
          if elem (v,c) vs1
          then if elem (v,c+n) vs2 then mkvar (v,c+n+1) else (v,c+n)
          else (v,c)
      v3 = mkvar v2;
   in
      (v2,v3):(mkrename (v3:vs1) n vs2)


rename :: [((String, Int), (String, Int))] -> Term -> Term
rename alist (Node (VSym v, ts)) =
    case find v alist of
        Just v2 -> Node (VSym v2, map (rename alist) ts)
        Nothing -> Node (VSym v, map (rename alist) ts)
rename alist (Node (a, ts)) =
    Node (a, map (rename alist) ts)


uniquevar :: (Rule, Rule) -> (Rule, Rule)
uniquevar ((l1, r1), (l2, r2)) = ((l1, r1), (rename alist l2, rename alist r2))
  where
    vars1 = vars l1
    n = findMax vars1
    alist = mkrename vars1 (n+1) (vars l2)

