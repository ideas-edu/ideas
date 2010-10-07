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
module Domain.Math.Derivative.Exercises 
   ( derivativeExercise, derivativePolyExercise
   ) where

import Common.Uniplate (universe)
import Control.Monad
import Prelude hiding (repeat, (^))
import Domain.Math.Derivative.Rules 
import Domain.Math.Derivative.Strategies
import Common.Library
import Domain.Math.Polynomial.Generators
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Numeric.Views
import Domain.Math.Examples.DWO5
import Domain.Math.Expr
import Test.QuickCheck

derivativePolyExercise :: Exercise Expr
derivativePolyExercise = describe
   "Find the derivative of a polynomial. First normalize the polynomial \
   \(e.g., with distribution). Don't make use of the product-rule, or \
   \other chain rules." $ makeExercise
   { exerciseId    = diffId # "polynomial"
   , status        = Provisional
   , parser        = parseExpr
   , isReady       = noDiff
   , isSuitable    = isPolyDiff
   , equivalence   = eqPolyDiff
   , similarity    = simPolyDiff
   , strategy      = derivativePolyStrategy
   , navigation    = navigator
   , examples      = concat (diffSet1 ++ diffSet2 ++ diffSet3)
   , testGenerator = Just $ liftM (diff . lambda (Var "x")) $ 
                        sized quadraticGen
   }

derivativeExercise :: Exercise Expr
derivativeExercise = makeExercise
   { exerciseId   = describe "Derivative" diffId
   , status       = Experimental
   , parser       = parseExpr
   , isReady      = noDiff
   , strategy     = derivativeStrategy
   , ruleOrdering = derivativeOrdering
   , navigation   = navigator
   , examples     = concat (diffSet1++diffSet2++diffSet3++diffSet4++
                            diffSet5++diffSet6++diffSet7++diffSet8)
   }

derivativeOrdering :: Rule a -> Rule b -> Ordering
derivativeOrdering x y = f x `compare` f y
 where
   f a = (getId a /= j, getId a == i, showId a)
   i = getId ruleDefRoot
   j = getId ruleDerivPolynomial

isPolyDiff :: Expr -> Bool
isPolyDiff = maybe False (`belongsTo` polyViewWith rationalView) . getDiffExpr

eqPolyDiff :: Expr -> Expr -> Bool
eqPolyDiff x y = 
   let f = applyD ruleDerivPolynomial
   in viewEquivalent (polyViewWith rationalView) (f x) (f y)

simPolyDiff :: Expr -> Expr -> Bool
simPolyDiff x y =
   let f = acExpr . cleanUpExpr2
   in f x == f y

noDiff :: Expr -> Bool
noDiff e = null [ () | Sym s _ <- universe e, s == diffSymbol ]   

-- go = checkExercise derivativePolyExercise