module Order where

import Util
import TRS 

type Mset a = [a]


{- Let "gsim" be quasi-ordering.
 - "mkgrter" convert "gsim" to strict partial ordering.
 - "mkeq" also convert it to equivalence relation.
 -}

mkgrter :: Ord a => (a -> a -> Bool) -> a -> a -> Bool
mkgrter gsim x y = gsim x y && not (gsim y x)

mkeq :: Ord a => (a -> a -> Bool) -> a -> a -> Bool
mkeq gsim x y = gsim x y && gsim y x


{-
     -- Partial ordering on Symbols
     --      You must prepare list which maps symbols to integers.
     --      For example,
     --           ol = [("0",1),("S",1),("P",3),("M",4),("F",5)]
     --      Other symbols without orderlist such as "x",
     --      have only trivial relation: "x" >= "x".
-}

eq_symbol :: Assoc String Int -> String -> String -> Bool
eq_symbol ol x y =
    if x == y then True
    else case (find x ol, find y ol) of
        (Just n, Just m) -> n == m
        _                -> False

grtereq_symbol :: Assoc String Int -> String -> String -> Bool
grtereq_symbol ol x y =
    if x == y then True
    else case (find x ol, find y ol) of 
        (Just n, Just m) -> n >= m
        _                -> False

grter_symbol :: Assoc String Int -> String -> String -> Bool
grter_symbol ol x y =
    case (find x ol, find y ol) of
        (Just n, Just m) -> n > m
        _                -> False


grtereq_lex :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> Bool
grtereq_lex p (x:xs) [] =True
grtereq_lex p [] y =False
grtereq_lex p (x:xs) (y:ys)  = 
    (not((x:xs)==[])&&(y:ys)==[])||p x y ||((x==y )&&grtereq_lex p xs ys) 

grtereq_lpo :: Assoc String Int -> Term -> Term -> Bool
grtereq_lpo t (Node(VSym x,y)) (Node(VSym m,n)) = 
    if (x==m)then True--eq_symbol t (prsym (VSym x)) (prsym (VSym m))
    else False
grtereq_lpo t (Node(x,y)) (Node(m,n))=(matchterm y (Node(m,n))) 
    ||(eq_symbol t (prsym x) (prsym m)&& (grtereq_lex (grtereq_lpo t) y  n) && xxx t (Node(x,y)) n)
    ||((grter_symbol t (prsym x) (prsym m))&& xxx t (Node(x,y)) n )
    ||three t y (Node(m,n))
    

prterms::[Term]->[String]
prterms [] =[] 
prterms (x:xs) =prterm x :prterms xs 
xxx :: Assoc String Int->Term->[Term]->Bool
xxx _ _ []=True
xxx t x (y:ys)=
    case mkgrter (grtereq_lpo t) x y of 
        (False)->False
        (True)->True&& xxx t x ys

three ::Assoc String Int-> [Term]->Term->Bool
three _ [] _=False
three t (x:xs) y=
    case (grtereq_lpo t x y) of
        True -> True 
        False->three t xs y
matchterm :: [Term] -> Term ->Bool
matchterm [] _ =False
matchterm (y:ys) p = if y==p then True else matchterm ys p

grter_lpo :: Assoc String Int -> Term -> Term -> Bool
grter_lpo t x y = mkgrter (grtereq_lpo t) x y



grtereq_rpo :: Assoc String Int -> Term -> Term -> Bool
grtereq_rpo t (Node(x,y)) (Node(m,n)) = (matchterm y (Node(m,n)))
    || ((eq_symbol t (prsym x) (prsym m))&&grtereq_mset (grter_symbol [])  y n)
    ||(not( (eq_symbol t (prsym x) (prsym m)))&& xxx2 t (Node(x,y)) n )
    ||(three2 t y (Node(m,n)))

xxx2 :: Assoc String Int->Term->[Term]->Bool
xxx2 _ _ []=True
xxx2 t x (y:ys)=mkgrter (grtereq_rpo t) x y && xxx t x ys  

three2 ::Assoc String Int-> [Term]->Term->Bool
three2 _ [] _=False
three2 t (x:xs) y=if(grtereq_rpo t x y) then True else three t xs y

grter_rpo :: Assoc String Int -> Term -> Term -> Bool
grter_rpo t x y=mkgrter (grtereq_rpo t) x y


grtereq_mset :: Ord a => (a -> a -> Bool) -> [Term] -> [Term] -> Bool
grtereq_mset p x y= y==[]
   || case msettest x y of
        (a,b)->big a b

big ::[Term]->[Term]->Bool
big a []=True
big [] b=False
big (x:xs) (y:ys)=
    if grter_symbol [("B",0),("A",1),("H",3),("G",3)] (prterm x) (prterm y) then
        big (x:xs) ys
    else big xs (y:ys)
msettest ::[Term]->[Term]->([Term],[Term])
msettest a []=(a,[]) 
msettest [] b=([],b) 
msettest (x:xs) (y:ys)= 
    if(elem x (y:ys)) then msettest (removeelment x (x:xs)) (removeelment x (y:ys)) 
    else (x:fst(msettest xs (y:ys)),snd(msettest xs (y:ys)))
removeelment ::Eq a => a->[a]->[a]
removeelment _ []=[]
removeelment x (y:ys)=
    if(x==y)then ys
    else y:removeelment x ys 
