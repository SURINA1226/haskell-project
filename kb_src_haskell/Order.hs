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
{- grtereq_lex ..... -}

grtereq_lpo :: Assoc String Int -> Term -> Term -> Bool
{- grtereq_lpo ..... -}
grter_lpo :: Assoc String Int -> Term -> Term -> Bool
{- grter_lpo .....   -}


grtereq_mset :: Ord a => (a -> a -> Bool) -> Mset a -> Mset a -> Bool
{- grtereq_mset ..... -}

grtereq_rpo :: Assoc String Int -> Term -> Term -> Bool
{- grtereq_rpo .....  -}
grter_rpo :: Assoc String Int -> Term -> Term -> Bool
{- grter_rpo .....    -}
