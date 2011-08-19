module Domain.Lambda where

import Common.Library
import Common.Utils.Uniplate

data Expr = Abs String Expr 
          | App Expr Expr 
          | Var String 
          | LApp Expr
          | Application
          | Composition deriving Eq
          
instance Uniplate Expr where
   uniplate (Abs x a)        = plate Abs |- x |* a -- ([a], \[b] -> Abs x b)
   uniplate (App f a)        = plate App |* f |* a -- ([f,a], \[f,a] -> App f a)
   uniplate (Var x)          = plate Var |- x          -- ([], \_ -> Var x)
   uniplate (LApp l)         = plate LApp |* l          -- ([l], \[l] -> LApp l)
   uniplate (Application)    = plate Application -- ([], \_ -> Application)
   uniplate (Composition)    = plate Composition -- ([], \_ -> Composition)

instance Show Expr where
   show (Abs x a) = "\\" ++ x ++ "." ++ show a
   show Application = "(:@:)"
   show Composition = "."
   show (LApp y) = "(" ++ show y ++ " :@:) " 
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
freeVars (LApp l)   = freeVars l
freeVars Application = []
freeVars Composition = []

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
introS = makeSimpleRule "intro-S" f
 where
   f (Abs x (App a b)) = Just $ App (App (Var "S") (Abs x a)) (Abs x b)
   f _ = Nothing

introK :: Rule Expr
introK = makeSimpleRule "intro-K" f
 where
   f (Abs x a) | x `notElem` freeVars a = Just $ App (Var "K") a
   f _ = Nothing

introI :: Rule Expr
introI = makeSimpleRule "intro-I" f
 where
   f (Abs x (Var y)) | x==y = Just $ Var "I"
   f _ = Nothing   

{-
s f g x = (f x) (g x)
k x y = x
i x = x
-}
  
pf :: Exercise Expr
pf = emptyExercise 
   { prettyPrinter = show
   , strategy      = pfStrategy
   , navigation    = navigator
   }

etaReduce :: Rule Expr 
etaReduce = makeSimpleRule "Eta_reduce" f
  where f (Abs x (App y z)) | z == Var x && x `notElem` freeVars y = Just $ LApp y
        f _ = Nothing

section2prefix :: Rule Expr
section2prefix = makeSimpleRule "Section_to_prefix" f
  where f (LApp y) = Just $ App Application y
        f _ = Nothing

pfStrategy :: LabeledStrategy (Context Expr)
pfStrategy = label "point-free" $ 
   repeatS $ somewhere $ alternatives $
      map liftToContext [etaReduce,section2prefix,introK]

ex1 :: Expr
ex1 = Abs "x" (Abs "y" (App (Var "x") (Var "y")))

ex2 :: Expr
ex2 = Abs "x" (Abs "y" (Var "x"))

main :: IO () 
main = printDerivation pf ex2
