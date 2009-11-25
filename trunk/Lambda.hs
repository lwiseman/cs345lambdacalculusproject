import Data.List
import System

-- abstract syntax tree
data Expr = Var String
          | App Expr Expr
          | Lambda String Expr deriving (Eq, Read)
instance Show Expr where
         show (Var x) = x
         show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
         show (Lambda x e) = "(\\" ++ x ++ " -> " ++ show e ++ ")"

-- return a list of free vars in an expression
freeVars :: Expr -> [Expr]
freeVars x@(Var _) = [x]
freeVars (App t s) = freeVars t ++ freeVars s
freeVars (Lambda x t) = [candidate | candidate <- freeVars t, candidate /= (Var x)]

-- substitute r for x in a, i.e. a[x := r]
substitute :: Expr -> Expr -> Expr -> Expr
substitute r (Var x) (Var y) | x == y = r
                             | otherwise = Var y
substitute r x@(Var _) (App t s) = App (substitute r x t) $ substitute r x s
substitute r (Var x) (Lambda y t) | x == y = Lambda y t
                                  | (Var y) `elem` freeVars r = Lambda (y ++ "'") $ substitute r (Var x) $ substitute (Var $ y ++ "'") (Var y) t -- TODO: this is a sloppy way to get new name, could conflict
                                  | otherwise = Lambda y $ substitute r (Var x) t

-- does alpha reduction via substitute when necessary
betaReduce :: Expr -> Expr
betaReduce (App (Lambda x t) s) = substitute s (Var x) t
betaReduce (App t s) = App (betaReduce t) $ betaReduce s
betaReduce (Lambda x t) = Lambda x $ betaReduce t
betaReduce r = r

-- return list of equivalent reductions to an expression's normal form
normalReduce :: Expr -> [Expr]
normalReduce r = unfoldr reduce r
                 where reduce r = let reduced = betaReduce r in if r == reduced then Nothing else Just (reduced, reduced)

-- get the normal form
normalize :: Expr -> Expr
normalize x = let result = normalReduce x
              in if result == []
                 then x
                 else last result

-- utility function
compose :: (Num t) => t -> (b -> b) -> b -> b
compose 0 f x = x
compose n f x = f $ compose (n - 1) f x

-- main loop
main :: IO ()
main = do fileName <- getArgs
          input <- readFile $ head fileName
          let content = head $ lines input
          let parsed = read content :: Expr
          let solution = normalize parsed
          putStr "> "
          print parsed

-- library fundamentals
zero :: Expr
zero = Lambda "f" $ Lambda "x" $ compose 0 (App (Var "f")) $ Var "x"
one :: Expr
one = Lambda "f" $ Lambda "x" $ compose 1 (App (Var "f")) $ Var "x"
two :: Expr
two = Lambda "f" $ Lambda "x" $ compose 2 (App (Var "f")) $ Var "x"
three :: Expr
three = Lambda "f" $ Lambda "x" $ compose 3 (App (Var "f")) $ Var "x"
four :: Expr
four = Lambda "f" $ Lambda "x" $ compose 4 (App (Var "f")) $ Var "x"
five :: Expr
five = Lambda "f" $ Lambda "x" $ compose 5 (App (Var "f")) $ Var "x"
six :: Expr
six = Lambda "f" $ Lambda "x" $ compose 6 (App (Var "f")) $ Var "x"
seven :: Expr
seven = Lambda "f" $ Lambda "x" $ compose 7 (App (Var "f")) $ Var "x"
eight :: Expr
eight = Lambda "f" $ Lambda "x" $ compose 8 (App (Var "f")) $ Var "x"
nine :: Expr
nine = Lambda "f" $ Lambda "x" $ compose 9 (App (Var "f")) $ Var "x"

inc :: Expr
inc = Lambda "n" (Lambda "f" (Lambda "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

plus :: Expr
plus = Lambda "x" (Lambda "y" (Lambda "s" (Lambda "z" (App (App (Var "x") (Var "s")) (App (App (Var "y") (Var "s")) (Var "z"))))))

times :: Expr
times = Lambda "x" (Lambda "y" (App (App (Var "x") (App plus (Var "y"))) zero))

dec :: Expr
dec = Lambda "n" (Lambda "s" (Lambda "z" (App (App (App (Var "n") (Lambda "n" (Lambda "f" (App (Var "f") (App (Var "n") (Var "s")))))) (Lambda "f" (Var "z"))) (Lambda "x" (Var "x")))))

true :: Expr
true = Lambda "x" (Lambda "y" (Var "x"))

false :: Expr
false = Lambda "x" (Lambda "y" (Var "y"))

fix :: Expr
fix = Lambda "f" (App (Lambda "x" (App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Lambda "x" (App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))))

ifThenElse :: Expr
ifThenElse = Lambda "p" (Lambda "a" (Lambda "b" (App (App (Var "p") (Var "a")) (Var "b"))))

isZero :: Expr
isZero = Lambda "n" (App (App (Var "n") (Lambda "x" false)) true)

mult :: Expr
mult = Lambda "m" (Lambda "n" (Lambda "f" (App (Var "n") (App (Var "m") (Var "f")))))

factorial :: Expr
factorial = App fix (Lambda "k" (Lambda "i" (App (App (App ifThenElse (App isZero (Var "i"))) one) (App (App mult (Var "i")) (App (Var "k") (App dec (Var "i")))))))

