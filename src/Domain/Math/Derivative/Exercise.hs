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
module Domain.Math.Derivative.Exercise (derivativeExercise) where

import Common.Uniplate (universe)
import Prelude hiding (repeat, (^))
import Domain.Math.Derivative.Rules 
import Domain.Math.Derivative.Strategies
import Common.Transformation
import Common.Navigator
import Common.Exercise
import Domain.Math.Examples.DWO5
import Domain.Math.Expr

derivativeExercise :: Exercise Expr
derivativeExercise = makeExercise
   { exerciseId   = describe "Derivative" diffId
   , status       = Experimental
   , parser       = parseExpr
   , isReady      = noDiff
   , strategy     = derivativeStrategy
   , ruleOrdering = derivativeOrdering
   , navigation   = navigator
   , examples     = [ex1, ex2, ex3, ex4] ++
                    concat (diffSet1++diffSet2++diffSet3++diffSet4++
                            diffSet5++diffSet6++diffSet7++diffSet8)
   }

derivativeOrdering :: Rule a -> Rule b -> Ordering
derivativeOrdering x y =
   let i = getId ruleDefRoot
   in (getId x == i, showId x) `compare` (getId y == i, showId y)

noDiff :: Expr -> Bool
noDiff e = null [ () | Sym s _ <- universe e, s == diffSymbol ]   

ex1, ex2, ex3 :: Expr
ex1 = diff $ lambda (Var "x") $ Var "x" ^ 2
ex2 = diff $ lambda (Var "x") $ ((1/3) :*: (x ^ fromInteger 3)) :+: (fromInteger (-3) :*: (x ^ fromInteger 2)) :+: x :+: fromInteger (-5)
 where x = Var "x"
ex3 = diff $ lambda (Var "x") (2 * Var "x") 
ex4 = diff $ lambda (Var "x") (ln (Var "x"))

go n = printDerivation derivativeExercise (examples derivativeExercise !! n)