module Domain.SKI where

import Common.Library
import Common.Uniplate

data Expr = Abs String Expr | App Expr Expr | Var String

instance Uniplate Expr where
   uniplate (Abs x a) = ([a], \[a] -> Abs x a)
   uniplate (App f a) = ([f,a], \[f,a] -> App f a)
   uniplate (Var x)   = ([], \_ -> Var x)

instance Show Expr where
   show (Abs x a) = "\\" ++ x ++ "." ++ show a
   show expr      = f expr
    where
      f (App a b) = f a ++ " " ++ g b
      f a         = g a
      g (Var x)   = x
      g a         = "(" ++ show a ++ ")"
 
freeVars :: Expr -> [String]
freeVars (Abs x a) = filter (/=x) (freeVars a)
freeVars (App a b) = freeVars a ++ freeVars b
freeVars (Var x)   = [x]

scomb, comp, flp :: Expr
scomb = Abs "f" $ Abs "g" $ Abs "x" $ App (App (Var "f") (Var "x")) 
                                          (App (Var "g") (Var "x"))
comp = Abs "f" $ Abs "g" $ Abs "x" $ App (Var "f") $ App (Var "g") (Var "x")
flp  = Abs "f" $ Abs "x" $ Abs "y" $ App (App (Var "f") (Var "y")) (Var "x")

-- \x -> \y -> x y
myex = Abs "x" $ Abs "y" $ App (Var "x") (Var "y")

ski :: Exercise Expr
ski = emptyExercise 
   { prettyPrinter = show
   , strategy      = skiStrategy
   , navigation    = navigator
   }

skiStrategy :: LabeledStrategy (Context Expr)
skiStrategy = label "ski" $ 
   repeatS $ somewhere $ alternatives $
      map liftToContext [introS, introK, introI]

introS :: Rule Expr
introS = undefined 

introK :: Rule Expr
introK = undefined

introI :: Rule Expr
introI = undefined
 
{-
s f g x = (f x) (g x)
k x y = x
i x = x
-}
  
main :: IO () 
main = printDerivation ski scomb
