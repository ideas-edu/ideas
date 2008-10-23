{-# OPTIONS -XFlexibleContexts #-}
module Domain.Programming.Eval where

import Control.Monad.State
import Control.Monad.Error
import Common.Uniplate
import Domain.Programming.Expr
import Domain.Programming.Prelude
import Test.QuickCheck
import qualified Data.Map as M

data EvalError = TypeError String
               | Undefined 
               | TimeOut
               | NoMessage
   deriving Show

instance Error EvalError where
   noMsg = NoMessage

typeError :: MonadError EvalError m => String -> m a
typeError = throwError . TypeError

type Eval = StateT Integer (Either EvalError)

maxTime :: Integer
maxTime = 1000000

-- To do: evaluate in Error monad to throw type errors, to report undefined, or
-- to time out the evaluation (for looping expressions)
eval :: Expr -> Either EvalError (Expr, Integer)
eval expr = runStateT (evalM expr) 0

evalM :: Expr -> Eval Expr
evalM expr = do
   i <- get
   if i>=maxTime then throwError TimeOut else put (i+1)
   case expr of
      MatchList b n c -> do
         eb <- evalM b
         case eb of
            Var "Nil" -> evalM n
            Apply (Apply (Var "Cons") x) xs -> evalM (Apply (Apply c x) xs)
            _ -> typeError "expecting a list"
      IfThenElse c t e -> do
         ec <- evalM c
         case ec of
            Var "True"  -> evalM t
            Var "False" -> evalM e
            _ -> typeError "expecting a boolean"
      Fix f -> do
         ef <- evalM f
         case ef of
            Lambda x e -> evalM (substExpr x (Fix f) e) 
            _ -> typeError "expecting a function"
      Apply f a -> do
         ef <- evalM f 
         case ef of
            Lambda s b -> evalM (substExpr s a b)
            Apply (Var g) x
               -- primitive binary functions
               | g == "Cons" -> liftM2 cons (evalM x) (evalM a)
               | g == "<="   -> do
                    p <- liftM2 (,) (evalM x) (evalM a)
                    case p of
                       (Int x, Int y) -> return (if x <= y then true else false)
                       _ -> typeError "expecting an int"
               | g == "==" -> do
                    p <- liftM2 (,) (evalM x) (evalM a)
                    case p of
                       (Int x, Int y) -> return (if x == y then true else false)
                       _ -> typeError "expecting an int"
            g -> return (Apply g a)
      Var x ->
         case M.lookup x prelude of
            Nothing
               | expr==undef -> throwError Undefined
               | otherwise   -> return (Var x)
            Just e  -> evalM e
      _ -> return expr
      
substExpr :: String -> Expr -> Expr -> Expr
substExpr s e (Var x) | x==s = e
substExpr s _ e@(Lambda x _) | x==s = e
substExpr s e expr = f $ map (substExpr s e) cs
 where (cs, f) = uniplate expr
 
---------------------------------------------------------------------
-- Testing

intlist :: [Int] -> Expr
intlist = foldr (cons . Int) nil

toBool :: Expr -> Bool
toBool (Var "True") = True
toBool (Var "False") = False
toBool _ = error "toBool"
 
-- assumes that no errors will occur
equivalent :: Expr -> Expr -> Bool
equivalent a b = 
   case (eval a, eval b) of
      (Right (x,_), Right (y,_)) -> x==y
      _ -> False
 
partially :: Expr -> Expr -> Bool
partially a b =
   case (eval a, eval b) of
      (Right (x,_), Right (y,_)) -> x==y
      (Right _, Left Undefined)  -> True
      _ -> False

prop1 :: [Int] -> Property
prop1 is = collect (length is) $
           case eval (sortedE # (isortE2 # intlist is)) of
              Right (e, _) -> toBool e
              Left e -> error $ show e

prop2 :: [Int] -> Property
prop2 is = collect (length is) $
           case eval (isPermE # (intlist is) # (isortE2 # intlist is)) of
              Right (e, _) -> toBool e
              Left e -> error $ show e
              
test1 = quickCheck prop1
test2 = quickCheck prop2


mylist = cons (Int 4) $ cons (Int 3) $ cons (Int 5) $ cons (Int 1) nil
test = eval (isPermE # mylist # (isortE2 # mylist))