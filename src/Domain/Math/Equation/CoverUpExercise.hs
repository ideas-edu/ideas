-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Equation.CoverUpExercise 
   ( coverUpExercise, coverUpStrategy 
   ) where

import Common.Library
import Data.Maybe
import Domain.Math.CleanUp (cleanUpExpr, cleanUpView)
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO1
import Domain.Math.Expr

------------------------------------------------------------
-- Exercise

coverUpExercise :: Exercise (OrList (Equation Expr))
coverUpExercise = makeExercise 
   { exerciseId   = describe "solve an equation by covering up" $
                       newId "algebra.equations.coverup"
   , status       = Provisional
   , parser       = parseExprWith (pOrList (pEquation pExpr))
   , equivalence  = eqCoverUp
   , similarity   = myEq
   , isReady      = solvedEquations
   , extraRules   = coverUpRulesOr
   , strategy     = coverUpStrategy
   , navigation   = termNavigator
   , examples     = map singleton (concat (fillInResult ++ coverUpEquations))
   }

------------------------------------------------------------
-- Strategy and rules
   
coverUpStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
coverUpStrategy = cleanUpStrategy (applyTop $ fmap $ fmap cleanUpExpr) $
   label "Cover-up" $
   repeatS $ somewhere $ alternatives coverUpRulesOr

eqCoverUp :: OrList (Equation Expr) -> OrList (Equation Expr) -> Bool
eqCoverUp a b = myEq (f a) (f b)
 where 
   inc = inContext coverUpExercise
   f x = fromMaybe x $ fromContext $ applyD coverUpStrategy $ inc x

myEq :: OrList (Equation Expr) -> OrList (Equation Expr) -> Bool 
myEq = viewEquivalent (traverseView (traverseView cleanUpView))