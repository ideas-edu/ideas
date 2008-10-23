module Domain.Programming.Prelude where

import Domain.Programming.Eval -- for testing
import Domain.Programming.Expr

infixl 5 #

(#) :: Expr -> Expr -> Expr
(#) = Apply
          
isortE :: Expr
isortE = Fix $ Lambda "f" $ Lambda "xs" $ MatchList (Var "xs")
   nil
   (Lambda "y" $ Lambda "ys" $ 
      insertE # Var "y" # (Var "f" # Var "ys"))

insertE :: Expr
insertE = Fix $ Lambda "f" $ Lambda "a" $ Lambda "xs" $ MatchList (Var "xs") 
   (cons (Var "a") nil)
   (Lambda "y" $ Lambda "ys" $ IfThenElse 
      (Var "<=" # Var "a" # Var "y") 
      (cons (Var "a") (cons (Var "y") (Var "ys"))) 
      (cons (Var "y") (Var "f" # Var "a" # Var "ys")))

sortedE :: Expr
sortedE = Fix $ Lambda "f" $ Lambda "xs" $ MatchList (Var "xs") 
   true
   (Lambda "y" $ Lambda "ys" $ MatchList (Var "ys")
      true
      (Lambda "z" $ Lambda "zs" $ andE # (Var "<=" # Var "y" # Var "z")
                                       # (Var "f" # (cons (Var "z") (Var "zs")))))

isPermE :: Expr
isPermE = Fix $ Lambda "f" $ Lambda "xs" $ Lambda "ys" $ MatchList (Var "xs") 
   (MatchList (Var "ys") true (Lambda "z" $ Lambda "zs" $ false))
   (Lambda "z" $ Lambda "zs" $ andE # (elemE # Var "z" # Var "ys") 
                                    # (Var "f" # Var "zs" # (deleteE # Var "z" # Var "ys")))

elemE :: Expr
elemE = Fix $ Lambda "f" $ Lambda "a" $ Lambda "xs" $ MatchList (Var "xs")
   false
   (Lambda "y" $ Lambda "ys" $ orE # (Var "==" # Var "a" # Var "y") # (Var "f" # Var "a" # Var "ys"))

deleteE :: Expr
deleteE = Fix $ Lambda "f" $ Lambda "a" $ Lambda "xs" $ MatchList (Var "xs") 
   nil
   (Lambda "y" $ Lambda "ys" $ IfThenElse (Var "==" # Var "a" # Var "y")
      (Var "ys")
      (cons (Var "y") (Var "f" # Var "a" # Var "ys")))
                                         
andE :: Expr
andE = Lambda "x" $ Lambda "y" $ IfThenElse (Var "x") (Var "y") false

orE :: Expr
orE = Lambda "x" $ Lambda "y" $ IfThenElse (Var "x") true (Var "y")

intlist = cons (Int 4) $ cons (Int 3) $ cons (Int 5) $ cons (Int 1) nil

test = eval (isPermE # intlist # (isortE # intlist))
 