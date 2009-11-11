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
import Common.Transformation
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
   , similarity   = eqRelation cleanUpSimple
   , equivalence  = viewEquivalent linearEquationView
   , isReady      = solvedEquation
   , extraRules   = linearRules
   , strategy     = mapRules ignoreContext linearStrategy
   , examples     = concat linearEquations
   }

quadraticExercise :: Exercise (OrList (Relation Expr))
quadraticExercise = makeExercise 
   { description  = "solve a quadratic equation"
   , exerciseCode = makeCode "math" "quadreq"
   , status       = Provisional
   , parser       = \input -> case parseWith (pOrList (pEquation pExpr)) input of
                                 Left err -> Left err
                                 Right xs -> Right (build (switchView equationView) xs)
   , similarity   = eqOrList cleanUpExpr2
   , equivalence  = error "equivalence" -- viewEquivalent quadraticEquationsView
   , isReady      = solvedRelations
   , extraRules   = map (ignoreContext . liftRule (switchView equationView)) $ 
                       quadraticRules ++ abcBuggyRules
   , strategy     = quadraticStrategy
   , examples     = map (orList . return . build equationView) (concat quadraticEquations)
   }
   
higherDegreeExercise :: Exercise (OrList (Relation Expr))
higherDegreeExercise = makeExercise 
   { description  = "solve an equation (higher degree)"
   , exerciseCode = makeCode "math" "higherdegree"
   , status       = Provisional
   , parser       = parser quadraticExercise
   , similarity   = eqOrList cleanUpExpr2
   , equivalence  = error "equivalence" -- viewEquivalent higherDegreeEquationsView
   , isReady      = solvedRelations
   , extraRules   = map (ignoreContext . liftRule (switchView equationView)) higherDegreeRules
   , strategy     = higherDegreeStrategy
   , examples     = map (orList . return . build equationView) 
                       (concat $ higherEq1 ++ higherEq2 ++ [higherDegreeEquations])
   }
   
quadraticNoABCExercise :: Exercise (OrList (Relation Expr))
quadraticNoABCExercise = quadraticExercise
   { description  = "solve a quadratic equation without abc-formula"
   , exerciseCode = makeCode "math" "quadreq-no-abc"
   , strategy     = configure cfg quadraticStrategy
   }
 where
   cfg = [ (ByName (name prepareSplitSquare), Expose)
         , (ByName "abc form", Hide)
         ]
         
quadraticWithApproximation :: Exercise (OrList (Relation Expr))
quadraticWithApproximation = quadraticExercise
   { description  = "solve a quadratic equation with approximation"
   , exerciseCode = makeCode "math" "quadreq-with-approx"
   , strategy     = configure cfg quadraticStrategy
   }
 where
   cfg = [ (ByName "approximate result", Expose)
         , (ByName "square root simplification", Hide)
         ]
   
--------------------------------------------
-- Equality

eqOrList :: (Relational f, Ord (f Expr)) => 
               (Expr -> Expr) -> OrList (f Expr) -> OrList (f Expr) -> Bool
eqOrList f x y = normOrList f x == normOrList f y

eqRelation :: (Relational f, Eq (f Expr)) => 
                 (Expr -> Expr) -> f Expr -> f Expr -> Bool
eqRelation f x y = normRelation f x == normRelation f y

normOrList :: (Relational f, Ord (f Expr)) => 
                 (Expr -> Expr) -> OrList (f Expr) -> OrList (f Expr)
normOrList f = normalize . fmap (normRelation f)

normRelation :: Relational f => (Expr -> Expr) -> f Expr -> f Expr
normRelation f rel
   | leftHandSide new > rightHandSide new && isSymmetric new = flipSides new
   | otherwise = new
 where
   new = fmap (normExpr f) rel

normExpr :: (Expr -> Expr) -> Expr -> Expr
normExpr f = normalizeWith [plusOperator, timesOperator] . f
 where
   plusOperator  = acOperator (+) isPlus
   timesOperator = acOperator (*) isTimes