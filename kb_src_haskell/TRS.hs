module TRS where

import Util
import Parsing

data Symbol = VSym (String, Int) | FSym String
  deriving(Eq,Ord)

data Term = Node (Symbol, [Term])
  deriving(Eq,Ord)

type Pattern    = Term
type Rule       = (Pattern, Pattern)
type RuleSet    = [Rule]
type Equation   = (Pattern, Pattern)
type EquationSet= [Equation]

type Subst = Assoc (String, Int) Term

tsize :: Term -> Int
tsize (Node(f,ts)) = 1 + tlistsize ts
tlistsize [] = 0
tlistsize (t:ts) = tsize t + tlistsize ts

depth(Node(VSym _,_))= 1
depth (Node(FSym x,ts)) = 1 + depthlist ts

depthlist [] = 0
depthlist (t:ts)   
    | depth(t) > max = depth(t)  
    | otherwise = max  
    where max = depthlist ts


varlist(Node(VSym x,_))= [x] 
varlist (Node(FSym _,ts)) =varlistlist(ts)
varlistlist [] = []
varlistlist(t :ts) = varlist(t) ++ varlistlist(ts)




-- =============
-- Parsing Term

{-  Comment on parsing defined in "Parsing.hs"
ident returns alpha-num that begins a lower letter
identifier is defined as   token ident
*Main> parse identifier " f (a,x)"
[("f","(a,x)")]
-}

-- alpha-num that begins an upper letter
uident :: Parser String
uident = do x <- uppernum
            xs <- many alphanum
            return (x:xs)

termfsym:: Parser Symbol
termfsym = do x <- token uident
              return (FSym x)
                
termvsym:: Parser Symbol
termvsym = do {x <- identifier;
               do {token(symbol ":"); n <- integer; return (VSym (x,n)) }
               <|> return (VSym (x,0)) }

{-
*Main> parse termsymbol "g ("
[(VSym ("g",0),"(")]
-}
termsymbol:: Parser Symbol
termsymbol = termfsym <|> termvsym

funterm:: Parser Term
funterm = do f <- termfsym
             token (symbol "(")
             tl <- termlist
             token (symbol ")")
             return (Node (f,tl))
cvterm:: Parser Term
cvterm = do f <- termsymbol
            return (Node (f,[]))

{-
*Main> parse term "F(x,y)"
[(Node (FSym "F",[Node (VSym ("x",0),[]),Node (VSym ("y",0),[])]),"")]
-}
term:: Parser Term
term = funterm <|> cvterm

termlist:: Parser [Term]
termlist = do {t <- term;
               do {token(symbol ","); tl <- termlist; return (t:tl)}
               <|> return [t] }

rule:: Parser Rule
rule = do t1 <- term
          token (symbol "->")
          t2 <- term
          return (t1, t2)

eq:: Parser Equation
eq = do t1 <- term
        token (symbol "=")
        t2 <- term
        return (t1, t2)

-----------------------------------------------------
-- registering Read function into Symbol, Term classes
instance Read Symbol where
  readsPrec _ s = parse termsymbol s

{-
read "F(x,y)"::Term
-}
instance Read Term where
  readsPrec _ s = parse term s


-------------------------------------
-- rdterm definition for compatibility
rdterm:: String -> Term
rdterm s = read s::Term

rdrule:: String -> Rule
rdrule s = (fst . head . parse rule) s

rdrules:: [String] -> RuleSet
rdrules ss = map rdrule ss

rdeq:: String -> Equation
rdeq s = (fst . head . parse eq) s

rdeqs:: [String] -> EquationSet
rdeqs ss = map rdeq ss


-- ===================
-- printing functions

-- Note that the second argument is an accumulator
showsSymbol:: Symbol -> ShowS
showsSymbol (VSym (x,i)) = (x ++) . 
            (if i<=0 then id else ((':':) . (shows i)))
showsSymbol (FSym f) = (f++)

showsTerm:: Term -> ShowS
showsTerm (Node (sym,tl)) =
  (shows sym) .
    (case tl of
      [] -> id
      t  -> ('(':) . showsTermlist t . (')':))

showsTermlist:: [Term] -> ShowS
showsTermlist [t] = shows t
showsTermlist (t:s:tl) = shows t . (',':) . showsTermlist (s:tl)

----------------------------------------------------
-- regitering Show function into Symbol, Term classes
instance Show Symbol where
    showsPrec _ s = showsSymbol s

instance Show Term where
    showsPrec _ s = showsTerm s

{-
*Main> show (read "FA(A,G(x,y))"::Term)
"FA(A,G(x:0,y:0))"
-}


-- prsym, prterm for compatibility
prsym:: Symbol -> String
prsym = show
prterm:: Term -> String
prterm = show

prrule:: Rule -> String
prrule (t1, t2) = (show t1) ++ " -> " ++ (show t2)

-- prrules:: RuleSet -> String
prrules [] = "[]\n"
prrules (r:rs) = "[ " ++ (prrule r) ++ (prrules0 rs)
prrules0 [] = " ]\n"
prrules0 (r:rs) = ",\n  " ++ (prrule r) ++ (prrules0 rs)

preq:: Equation -> String
preq (t1, t2) = (show t1) ++ " = " ++ (show t2)

preqs:: EquationSet -> String
preqs [] = "[]\n"
preqs (e:es) = "[ " ++ (preq e) ++ (preqs0 es)
preqs0 [] = " ]\n"
preqs0 (e:es) = ",\n  " ++ (preq e) ++ (preqs0 es)

prsubst:: Subst -> String
prsubst [] = "[]\n"
prsubst (((x,i),t):ss) = "[ " ++ (show (Node(VSym (x,i), []))) ++ " := " ++ (show t) ++ (prsubst0 ss)
prsubst0 [] = " ]\n"
prsubst0 (((x,i),t):ss) = ",\n  " ++ (show (Node(VSym (x,i), []))) ++ " := " ++ (show t) ++ (prsubst0 ss)
