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
module Domain.Math.Polynomial.Exercises where

import Common.Context
import Common.Exercise
import Common.Rewriting
import Common.Strategy
import Common.View
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO1
import Domain.Math.Examples.DWO2
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Polynomial.BuggyRules
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views

------------------------------------------------------------
-- Exercises

linearExercise :: Exercise (Equation Expr)
linearExercise = makeExercise 
   { description  = "solve a linear equation"
   , exerciseCode = makeCode "math" "lineq"
   , status       = Provisional
   , parser       = parseWith (pEquation pExpr)
   , similarity   = eqEquation cleanUpSimple
   , equivalence  = viewEquivalent linearEquationView
   , isReady      = solvedEquation
   , extraRules   = linearRules
   , strategy     = mapRules ignoreContext linearStrategy
   , examples     = concat linearEquations
   }

quadraticExercise :: Exercise (OrList (Equation Expr))
quadraticExercise = makeExercise 
   { description  = "solve a quadratic equation"
   , exerciseCode = makeCode "math" "quadreq"
   , status       = Provisional
   , parser       = parseWith (pOrList (pEquation pExpr))
   , similarity   = eqOrList cleanUpExpr2
   , equivalence  = viewEquivalent quadraticEquationsView
   , isReady      = solvedEquations
   , extraRules   = map ignoreContext $ quadraticRules ++ abcBuggyRules
   , strategy     = mapRules ignoreContext (quadraticStrategy True)
   , examples     = map (orList . return) (concat quadraticEquations)
   }
   
higherDegreeExercise :: Exercise (OrList (Equation Expr))
higherDegreeExercise = makeExercise 
   { description  = "solve an equation (higher degree)"
   , exerciseCode = makeCode "math" "higherdegree"
   , status       = Provisional
   , parser       = parseWith (pOrList (pEquation pExpr))
   , similarity   = eqOrList cleanUpExpr2
   , equivalence  = viewEquivalent higherDegreeEquationsView
   , isReady      = solvedEquations
   , extraRules   = map ignoreContext higherDegreeRules
   , strategy     = mapRules ignoreContext higherDegreeStrategy
   , examples     = map (orList . return) (concat $ higherEq1 ++ higherEq2 ++ [higherDegreeEquations])
   }
   
quadraticNoABCExercise :: Exercise (OrList (Equation Expr))
quadraticNoABCExercise = quadraticExercise
   { description  = "solve a quadratic equation without abc-formula"
   , exerciseCode = makeCode "math" "quadreq-no-abc"
   , strategy     = mapRules ignoreContext (quadraticStrategy False) 
   }
   
--------------------------------------------
-- Equality

eqOrList :: (Expr -> Expr) -> OrList (Equation Expr) -> OrList (Equation Expr) -> Bool
eqOrList f x y = normOrList f x == normOrList f y

eqEquation :: (Expr -> Expr) -> Equation Expr -> Equation Expr -> Bool
eqEquation f x y = normEquation f x == normEquation f y

normOrList :: (Expr -> Expr) -> OrList (Equation Expr) -> OrList (Equation Expr)
normOrList f = normalize . fmap (normEquation f)

normEquation :: (Expr -> Expr) -> Equation Expr -> Equation Expr
normEquation f eq
   | a <= b    = a :==: b
   | otherwise = b :==: a
 where
   a :==: b = fmap (normExpr f) eq

normExpr :: (Expr -> Expr) -> Expr -> Expr
normExpr f = normalizeWith [plusOperator, timesOperator] . f
 where
   plusOperator  = acOperator (+) isPlus
   timesOperator = acOperator (*) isTimes