module Domain.Programming.Prelude where

import Data.Map as M
import Domain.Programming.Expr
import Domain.Programming.Sorting

infixl 5 #

type Environment = M.Map String Expr

prelude :: Environment 
prelude = M.fromList
   [ ("foldr",  foldrE)
   , ("(&&)",   andE)
   , ("(||)",   orE)
   , ("elem",   elemE)
   , ("delete", deleteE)
   , ("take",   takeE)
   , ("drop",   dropE)
   , ("length", lengthE)
   , ("minimum",minimumE)
   ]

(#) :: Expr -> Expr -> Expr
(#) = Apply
    
foldrE :: Expr 
foldrE = Lambda "op" $ Lambda "e" $ Fix $ Lambda "rec" $
   Lambda "xs" $ MatchList (Var "xs") 
      (Var "e")
      (Lambda "y" $ Lambda "ys" $ Var "op" # Var "y" # (Var "rec" # Var "ys"))

elemE :: Expr
elemE = Fix $ Lambda "f" $ Lambda "a" $ Lambda "xs" $ MatchList (Var "xs")
   false
   (Lambda "y" $ Lambda "ys" $ Var "(||)" # (Var "==" # Var "a" # Var "y") # (Var "f" # Var "a" # Var "ys"))

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

takeE :: Expr
takeE = Fix $ Lambda "f" $ Lambda "i" $ Lambda "xs" $ IfThenElse (Var "==" # Var "i" # Int 0)
   nil
   (MatchList (Var "xs") 
       nil
       (Lambda "y" $ Lambda "ys" $ cons (Var "y") (Var "f" # (Var "-" # Var "i" # Int 1) # Var "ys")))

dropE :: Expr
dropE = Fix $ Lambda "f" $ Lambda "i" $ Lambda "xs" $ IfThenElse (Var "==" # Var "i" # Int 0)
   (Var "xs")
   (MatchList (Var "xs") 
       nil
       (Lambda "y" $ Lambda "ys" $ Var "f" # (Var "-" # Var "i" # Int 1) # Var "ys"))
       
lengthE :: Expr
lengthE = Var "foldr" # op # Int 0
 where op = Lambda "x" $ Var "+" # Int 1
     

minimumE :: Expr
minimumE = Var "foldr" # op # Int (maxBound::Int)
  where op = Lambda "x" $ Lambda "y" $ IfThenElse (Var "<=" # Var "x" # Var "y") 
                                         (Var "x") 
                                         (Var "y")
  
---------------------------------------------------------------
-- Sorting algorithms

isortE2 :: Expr
isortE2 = Var "foldr" # insertE # nil 
                
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

msortE :: Expr
msortE = Fix $ Lambda "f" $ Lambda "xs" $ MatchList (Var "xs") 
   nil
   (Lambda "y" $ Lambda "ys" $ MatchList (Var "ys") 
      (cons (Var "y") nil)
      (Lambda "z" $ Lambda "zs" $ 
         mergeE # (Var "f" # (Var "take" # mid # Var "xs")) 
                # (Var "f" # (Var "drop" # mid # Var "xs"))))
 where
  mid = Var "div" # (Var "length" # Var "xs") # Int 2

mergeE :: Expr
mergeE = Fix $ Lambda "f" $ Lambda "as" $ Lambda "bs" $ MatchList (Var "as")
   (Var "bs")
   (Lambda "x" $ Lambda "xs" $ MatchList (Var "bs")
      (Var "as") 
      (Lambda "y" $ Lambda "ys" $ IfThenElse (Var "<=" # Var "x" # Var "y")
         (cons (Var "x") (Var "f" # Var "xs" # Var "bs"))
         (cons (Var "y") (Var "f" # Var "as" # Var "ys")))) 

ssortE :: Expr
ssortE = Fix $ Lambda "f" $ Lambda "l"  $ MatchList (Var "l")
   nil
   (Lambda "x" $ Lambda "xs" $ cons (Var "minimum" # cons (Var "x") (Var "xs")) 
                                    (Var "f" # (Var "delete" # (Var "minimum" # cons (Var "x") (Var "xs")) 
                                                             # cons (Var "x") (Var "xs"))))

-----------------------------------------------------------------
-- Contracts for sorting

sortedE :: Expr
sortedE = Fix $ Lambda "f" $ Lambda "xs" $ MatchList (Var "xs") 
   true
   (Lambda "y" $ Lambda "ys" $ MatchList (Var "ys")
      true
      (Lambda "z" $ Lambda "zs" $ Var "(&&)" # (Var "<=" # Var "y" # Var "z")
                                             # (Var "f" # (cons (Var "z") (Var "zs")))))

isPermE :: Expr
isPermE = Fix $ Lambda "f" $ Lambda "xs" $ Lambda "ys" $ MatchList (Var "xs") 
   (MatchList (Var "ys") true (Lambda "z" $ Lambda "zs" $ false))
   (Lambda "z" $ Lambda "zs" $ Var "(&&)" # (Var "elem" # Var "z" # Var "ys") 
                                          # (Var "f" # Var "zs" # (Var "delete" # Var "z" # Var "ys")))