module Domain.Math.Polynomial.Exercises where

import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.CleanUp
import Common.Exercise
import Common.Rewriting
import Domain.Math.Data.Equation
import Domain.Math.Equation.Views
import Domain.Math.Expr
import Domain.Math.Data.OrList
import Domain.Math.ExercisesDWO
import Domain.Math.Expr.Parser
import Common.View
import Common.Context
import Data.List

------------------------------------------------------------
-- Exercises

linearExercise :: Exercise (Equation Expr)
linearExercise = makeExercise 
   { identifier    = "lineq"
   , domain        = "math"
   , description   = "solve a linear equation"
   , status        = Provisional
   , parser        = parseWith (pEquation pExpr)
   , equality      = eqEquation cleanUpSimple
   , equivalence   = viewEquivalent linearEquationView
   , finalProperty = solvedEquation
   , ruleset       = linearRules
   , strategy      = ignoreContext linearStrategy
   , termGenerator = ExerciseList (concat linearEquations)
   }

quadraticExercise :: Exercise (OrList (Equation Expr))
quadraticExercise = makeExercise 
   { identifier    = "quadreq"
   , domain        = "math"
   , description   = "solve a quadratic equation"
   , status        = Provisional
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = eqOrList cleanUpExpr
   , equivalence   = viewEquivalent quadraticEquationsView
   , finalProperty = solvedEquations
   , ruleset       = map ignoreContext quadraticRules
   , strategy      = ignoreContext quadraticStrategy
   , termGenerator = ExerciseList (map (orList . return) $ concat quadraticEquations)
   }
   
higherDegreeExercise :: Exercise (OrList (Equation Expr))
higherDegreeExercise = makeExercise 
   { identifier    = "higherdegree"
   , domain        = "math"
   , description   = "solve an equation (higher degree)"
   , status        = Provisional
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = eqOrList cleanUpExpr
   , equivalence   = viewEquivalent higherDegreeEquationsView
   , finalProperty = solvedEquations
   , ruleset       = map ignoreContext higherDegreeRules
   , strategy      = ignoreContext higherDegreeStrategy
   , termGenerator = ExerciseList (map (orList . return) higherDegreeEquations)
   }
   
--------------------------------------------
-- Equality

eqOrList :: (Expr -> Expr) -> OrList (Equation Expr) -> OrList (Equation Expr) -> Bool
eqOrList f x y = normOrList f x == normOrList f y

eqEquation :: (Expr -> Expr) -> Equation Expr -> Equation Expr -> Bool
eqEquation f x y = normEquation f x == normEquation f y

normOrList :: (Expr -> Expr) -> OrList (Equation Expr) -> OrList (Equation Expr)
normOrList f ors = 
   case disjunctions ors of 
      Just xs -> orList $ nub $ sort $ map (normEquation f) xs
      Nothing -> false

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