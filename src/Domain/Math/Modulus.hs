module Domain.Math.Modulus where

import Prelude hiding (repeat)
import Common.Apply
import Common.Strategy
import Common.Uniplate
import Common.Transformation
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Equation
import Domain.Math.OrList
import Domain.Math.Views
import Domain.Math.ExercisesDWO
import Domain.Math.HigherDegreeEquations (equationsStrategy)
import Domain.Math.QuadraticEquations (solvedList, forOne)
{- 
Simplifications:
   abs r = r    if r>=0
           -r   if r<0
   abs (abs e)  = abs e
   abs (-e)     = abs e
   abs (sqrt e) = sqrt e
   abs (e^n)    = e^n  if even n

Distribution:
   abs (a*b) = abs a * abs b
   abs (a/b) = abs a / abs b

Equations:
   abs a = b        =   a = b or a = -b
   abs a = abs b    =   a = b or a = -b    (special case)
-}

-- For level-4 exercises, I need cube-roots (to solve x^3=9)
main = flip mapM_ (take 12 $ concat modulusEquations) $ \eq -> 
   let res = applyD modulusStrategy (OrList [eq])
   in if solvedList res then print "ok" else print res

modulusStrategy :: LabeledStrategy (OrList (Equation Expr))
modulusStrategy = label "modulus" $ noAbs <*> try (check p <*> equationsStrategy <*> repeat inconsistencies)
 where p (OrList xs) = all absFree $ concat [[a,b] | a:==: b <- xs]
       absFree e = null [ () | Sym "abs" [_] <- universe e ]

noAbs :: LabeledStrategy (OrList (Equation Expr))
noAbs = label "remove modulus" $ repeat absEquation

------------------------------------------------------------------------
-- Equation rules

absEquation :: Rule (OrList (Equation Expr))
absEquation = makeSimpleRuleList "abs in equation" $ forOne f
 where
   f (Sym "abs" [a] :==: Sym "abs" [b]) = Just [a :==: b, a :==: -b]
   f (Sym "abs" [a] :==: b            ) = Just [a :==: b, a :==: -b]
   f (a             :==: Sym "abs" [b]) = Just [a :==: b, -a :==: b]
   f _ = Nothing
   
-- remove inconsistent equations from the or-list, such as 0==1
-- TODO: move this to a different module (should not be here)
inconsistencies :: Rule (OrList (Equation Expr))
inconsistencies = makeSimpleRuleList "inconsistencies" $ forOne $ \(a :==: b) -> do
   r1 <- match rationalView a
   r2 <- match rationalView b
   guard (r1 /= r2)
   return []