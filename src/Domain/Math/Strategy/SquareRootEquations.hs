-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Strategy.SquareRootEquations where

import Prelude hiding (repeat)
import Common.Apply
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Traversable
import Common.Uniplate (universe)
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.View.Basic
import Domain.Math.ExercisesDWO
import Domain.Math.Polynomial.HigherDegreeEquations (equationsStrategy)
import Domain.Math.Polynomial.QuadraticEquations (solvedList)
{- 
Strategie (p21. G&R deel 1):
1) Isoleer wortel
2) Kwadrateer
3) Controleer

Equations:
   sqrt a = b   =   a = b^2   (if a >= 0, and b>=0)
-}

main = forM_ (concat sqrtEquations) $ \eq -> 
   let res = applyD sqrtStrategy (OrList [eq])
   in if solvedList res then print "ok" else print res

-- TODO: check results afterwards (invalid results due to squaring both sides)
sqrtStrategy :: LabeledStrategy (OrList (Equation Expr))
sqrtStrategy = label "squareroot equation" $ isolate <*> repeat squareBoth <*>
   try (check p <*> equationsStrategy <*> repeat inconsistencies)
 where p (OrList xs) = all (not . varUnderSqrt) $ concat [[a,b] | a:==: b <- xs]

isolate :: LabeledStrategy (OrList (Equation Expr))
isolate = label "isolate" $ repeat $ alternatives 
   [coverUpPlus, coverUpTimes, coverUpNegation]

varUnderSqrt :: Expr -> Bool
varUnderSqrt e = not $ null [ () | Sqrt a <- universe e, hasVars a ]

------------------------------------------------------------------------
-- Equation rules

-- Isolation
coverUpPlus :: Rule (OrList (Equation Expr))
coverUpPlus  = makeSimpleRule "coverup plus" $ onceJoinM $ \(a :==: b) -> do
   guard (not $ varUnderSqrt b)
   (x, y) <- match plusView a
   case (varUnderSqrt x, varUnderSqrt y) of
      (True,  False) -> return $ OrList [x :==: b .-. y]
      (False, True ) -> return $ OrList [y :==: b .-. x]
      _ -> Nothing

coverUpNegation :: Rule (OrList (Equation Expr))
coverUpNegation = makeSimpleRule "coverup negation" $ onceJoinM f 
 where
   f (Negate a :==: b) | varUnderSqrt a && not (varUnderSqrt b) =
      return $ OrList [a :==: Negate b]
   f _ = Nothing
      
coverUpTimes :: Rule (OrList (Equation Expr))
coverUpTimes = makeSimpleRule "coverup times" $ onceJoinM $ \(a :==: b) -> do
   guard (not $ varUnderSqrt b)
   (x, y) <- match timesView a
   case (varUnderSqrt x, varUnderSqrt y) of
      (True,  False) -> return $ OrList [x :==: b ./. y]
      (False, True ) -> return $ OrList [y :==: b ./. x]
      _ -> Nothing

--------------------------------------------------------------

squareBoth :: Rule (OrList (Equation Expr))
squareBoth = makeSimpleRule "inconsistencies" $ onceJoinM f
 where
    f (Sqrt a :==: Sqrt b) = return $ OrList [a :==: b]
    f (a      :==: Sqrt b) = return $ OrList [a .^. 2 :==: b]
    f (Sqrt a :==: b     ) = return $ OrList [a :==: b .^. 2]
    f _ = Nothing

-- remove inconsistent equations from the or-list, such as 0==1
-- TODO: move this to a different module (should not be here)
inconsistencies :: Rule (OrList (Equation Expr))
inconsistencies = makeSimpleRule "inconsistencies" $ onceJoinM $ \(a :==: b) -> do
   r1 <- match rationalView a
   r2 <- match rationalView b
   guard (r1 /= r2)
   return $ OrList []