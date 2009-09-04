module Domain.Math.Polynomial.Exercises where

import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views
import Common.Exercise
import Domain.Math.Data.Equation
import Domain.Math.Equation.Views
import Domain.Math.Expr
import Domain.Math.Data.OrList
import Domain.Math.ExercisesDWO
import Domain.Math.Expr.Parser
import Common.View
import Common.Context

------------------------------------------------------------
-- Exercises

linearExercise :: Exercise (Equation Expr)
linearExercise = makeExercise 
   { identifier    = "lineq"
   , domain        = "math"
   , description   = "solve a linear equation"
   , status        = Experimental
   , parser        = parseWith (pEquation pExpr)
   , equality      = (==)
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
   , status        = Experimental
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = (==) 
   , equivalence   = viewEquivalent quadraticEquationsView
   , finalProperty = solvedEquations
   , ruleset       = map ignoreContext quadraticRules
   , strategy      = ignoreContext quadraticStrategy
   , termGenerator = ExerciseList (map (OrList . return) $ concat quadraticEquations)
   }
   
higherDegreeExercise :: Exercise (OrList (Equation Expr))
higherDegreeExercise = makeExercise 
   { identifier    = "higherdegree"
   , domain        = "math"
   , description   = "solve an equation (higher degree)"
   , status        = Experimental
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = (==) 
   , equivalence   = viewEquivalent higherDegreeEquationsView
   , finalProperty = solvedEquations
   , ruleset       = map ignoreContext higherDegreeRules
   , strategy      = ignoreContext higherDegreeStrategy
   , termGenerator = ExerciseList (map (OrList . return) higherDegreeEquations)
   }