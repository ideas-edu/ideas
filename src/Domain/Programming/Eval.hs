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

binOps :: M.Map String (Int -> Int -> Expr)
binOps = M.fromList 
   [ ("<=",  \x y -> if x<=y then true else false) 
   , ("==",  \x y -> if x==y then true else false) 
   , ("-" ,  \x y -> Int (x-y))
   , ("+" ,  \x y -> Int (x+y))
   , ("*" ,  \x y -> Int (x*y))
   , ("div", \x y -> Int (div x y))
   ]

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
               | otherwise   ->
                    case M.lookup g binOps of 
                       Just f -> do
                          p <- liftM2 (,) (evalM x) (evalM a)
                          case p of
                             (Int x, Int y) -> return (f x y)
                             _ -> typeError "expecting an int"
                       Nothing -> return (Apply ef a)
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
substExpr s e expr = descend (substExpr s e) expr
 
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

prop1E2 :: [Int] -> Property
prop1E2 is = collect (length is) $
           case eval (sortedE # (isortE2 # intlist is)) of
              Right (e, _) -> toBool e
              Left e -> error $ show e

prop2E2 :: [Int] -> Property
prop2E2 is = collect (length is) $
           case eval (isPermE # (intlist is) # (isortE2 # intlist is)) of
              Right (e, _) -> toBool e
              Left e -> error $ show e
              
prop1E3 :: [Int] -> Property
prop1E3 is = collect (length is) $
           case eval (sortedE # (isortE3 # intlist is)) of
              Right (e, _) -> toBool e
              Left e -> error $ show e

prop2E3 :: [Int] -> Property
prop2E3 is = collect (length is) $
           case eval (isPermE # (intlist is) # (isortE3 # intlist is)) of
              Right (e, _) -> toBool e
              Left e -> error $ show e

test1 = quickCheck prop1E2
test2 = quickCheck prop2E2
test3 = quickCheck prop1E3
test4 = quickCheck prop2E3


mylist = cons (Int 4) $ cons (Int 3) $ cons (Int 5) $ cons (Int 1) nil
mytest = eval (isPermE # mylist # (isortE2 # mylist))

q = eval $ mergeE # intlist [1,3,4] # intlist [2,5]