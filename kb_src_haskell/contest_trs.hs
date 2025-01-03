import Util
import TRS
import Reduce
import Data.Time
import Data.Char          (isDigit)
import System.Environment (getArgs)


mkterm 0 = Node (FSym "O", [])
mkterm n = Node (FSym "S", [mkterm (n-1)])

rfib = rdrules 
    ["Add(O, y) -> y",
     "Add(S(x), y) -> S(Add(x, y))",
     "Fib(O) -> O",
     "Fib(S(O)) -> S(O)",
     "Fib(S(S(x))) -> Add(Fib(x), Fib(S(x)))"]

rfact = rdrules
    ["Add(O, y) -> y",
     "Add(S(x), y) -> S(Add(x, y))",
     "Mul(O, y) -> O",
     "Mul(S(x), y) -> Add(Mul(x, y), y)",
     "Fact(O) -> S(O)",
     "Fact(S(x)) -> Mul(S(x), Fact(x))"]

t_fib15 = Node (FSym "Fib", [mkterm 15])
t_fact6 = Node (FSym "Fact", [mkterm 6])

test n =
    case n of
        1 -> linf rfib t_fib15
        2 -> ponf rfib t_fib15
        3 -> lonf rfib t_fib15
        4 -> linf rfact t_fact6
        5 -> ponf rfact t_fact6
        6 -> lonf rfact t_fact6

main :: IO ()
main = do
    let isDigitOnly cs = foldl (\b c -> b && isDigit c ) True cs
    args <- getArgs
    let cs = (args !! 0)
    if isDigitOnly cs
    then do
        let n = (read cs :: Int)
        x <- getCurrentTime
        print $ test n
        y <- getCurrentTime
        print $ diffUTCTime y x
    else
        print "wrong argument"
