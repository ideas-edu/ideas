module Domain.Programming.Eval where

import Common.Uniplate
import Domain.Programming.Expr

-- To do: evaluate in Error monad to throw type errors, to report undefined, or
-- to time out the evaluation (for looping expressions)
eval :: Expr -> Expr
eval expr = 
   case expr of
      MatchList b n c -> 
         case eval b of
            Var "Nil" -> eval n
            Apply (Apply (Var "Cons") x) xs -> eval (Apply (Apply c x) xs)
            e -> error $ "Type error: expecting list instead of " ++ show e
      IfThenElse c t e -> 
         case eval c of
            Var "True" -> eval t
            Var "False" -> eval e
            x -> error $ "Type error: expecting boolean"
      Fix f -> 
         case eval f of
            Lambda x e -> eval (substExpr x (Fix f) e) 
            a -> error "Type error: expecting a function"
      Apply f a -> 
         case eval f of
            Lambda s b -> eval (substExpr s a b)
            Apply (Var g) x
               -- primitive binary functions
               | g == "Cons" -> cons (eval x) (eval a)
               | g == "<="   -> 
                    case (eval x, eval a) of
                       (Int x, Int y) -> if x <= y then true else false
                       _ -> error $ "Type error: expecting int"
               | g == "==" ->
                    case (eval x, eval a) of
                       (Int x, Int y) -> if x == y then true else false
                       _ -> error $ "Type error: expecting int"
            g -> Apply g a
      _ -> expr
      
substExpr :: String -> Expr -> Expr -> Expr
substExpr s e (Var x) | x==s = e
substExpr s _ e@(Lambda x _) | x==s = e
substExpr s e expr = f $ map (substExpr s e) cs
 where (cs, f) = uniplate expr