module Domain.Math.Strategy.Modulus where

import Prelude hiding (repeat)
import Common.Apply
import Common.Strategy
import Common.Uniplate
import Common.Transformation
import Common.Traversable
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.View.Basic
import Domain.Math.ExercisesDWO
import Domain.Math.Polynomial.HigherDegreeEquations (equationsStrategy)
import Domain.Math.Polynomial.QuadraticEquations (solvedList)
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
       absFree e = null [ () | Sym s [_] <- universe e, s == absSymbol ]

noAbs :: LabeledStrategy (OrList (Equation Expr))
noAbs = label "remove modulus" $ repeat absEquation

------------------------------------------------------------------------
-- Equation rules

absEquation :: Rule (OrList (Equation Expr))
absEquation = makeSimpleRule "abs in equation" $ onceJoinM f
 where
   f (Sym s1 [a] :==: Sym s2 [b]) | all (==absSymbol) [s1, s2] = 
      Just $ OrList [a :==: b, a :==: -b]
   f (Sym s1 [a] :==: b         ) | s1 == absSymbol = 
      Just $ OrList [a :==: b, a :==: -b]
   f (a          :==: Sym s2 [b]) | s2 == absSymbol = 
      Just $ OrList [a :==: b, -a :==: b]
   f _ = Nothing
   
-- remove inconsistent equations from the or-list, such as 0==1
-- TODO: move this to a different module (should not be here)
inconsistencies :: Rule (OrList (Equation Expr))
inconsistencies = makeSimpleRule "inconsistencies" $ onceJoinM $ \(a :==: b) -> do
   r1 <- match rationalView a
   r2 <- match rationalView b
   guard (r1 /= r2)
   return $ OrList []