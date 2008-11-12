module Domain.Programming.Expr where

import Common.Uniplate

data Expr = -- lambda calculus plus fixpoint
            Lambda String Expr
          | Var String
          | Apply Expr Expr
          | Fix Expr
            -- integers
          | Int Int
            -- booleans
          | IfThenElse Expr Expr Expr
            -- lists
          | MatchList Expr{-body-} Expr{-nil-} Expr{-cons-}
   deriving (Show, Eq, Read)

instance Uniplate Expr where
   uniplate expr =
      case expr of
         Lambda s e       -> ([e], \[e] -> Lambda s e)
         Var s            -> ([], \[] -> expr)
         Apply f a        -> ([f,a], \[f,a] -> Apply f a)
         Fix f            -> ([f], \[f] -> Fix f)
         Int n            -> ([], \[] -> expr)
         IfThenElse c t e -> ([c,t,e], \[c,t,e] -> IfThenElse c t e)
         MatchList e n c  -> ([e,n,c], \[e,n,c] -> MatchList e n c)

collectVars :: Expr -> [String]
collectVars e = [ s | Lambda s _ <- universe e ]

makeLet :: String -> Expr -> Expr -> Expr
makeLet s e body = Apply (Lambda s body) (Fix (Lambda s e))
 
true, false :: Expr
true  = Var "True"
false = Var "False"

cons :: Expr -> Expr -> Expr
cons x xs = Apply (Apply (Var "Cons") x) xs

nil :: Expr 
nil = Var "Nil" 

undef :: Expr
undef = Var "undefined"

{-
testje1 = isortExpr == buildExpr (getRules isortExpr)
testje2 = isortExpr == fromContext (applyD isortStrategy (inContext undef))
-}