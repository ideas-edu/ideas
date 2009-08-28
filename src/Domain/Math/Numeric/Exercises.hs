module Domain.Math.Numeric.Exercises    
   ( naturalExercise, integerExercise
   , rationalExercise, fractionExercise
   ) where
   
import Common.Exercise
import Common.Strategy
import Common.View
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Numeric.Strategies
import Domain.Math.Numeric.Views
import Domain.Math.ExercisesDWO (calculateResults)
import Common.Context
   
------------------------------------------------------------
-- Exercises

numericExercise :: LabeledStrategy Expr -> Exercise Expr
numericExercise s = makeExercise 
   { domain        = "math"
   , status        = Experimental
   , parser        = parseExpr
   , equality      = (==)
   , equivalence   = viewEquivalent rationalView
   , ruleset       = rulesInStrategy (liftToContext s)
   , strategy      = liftToContext s
   }

naturalExercise :: Exercise Expr
naturalExercise = (numericExercise naturalStrategy)
   { identifier    = "natural"
   , description   = "simplify expression (natural numbers)"
   , finalProperty = (`belongsTo` integerNormalForm)
   , termGenerator = ExerciseList (concat calculateResults)
   }

integerExercise :: Exercise Expr
integerExercise = (numericExercise integerStrategy)
   { identifier    = "integer"
   , description   = "simplify expression (integers)"
   , finalProperty = (`belongsTo` integerNormalForm)
   , termGenerator = ExerciseList (concat calculateResults)
   }
   
rationalExercise :: Exercise Expr
rationalExercise = (numericExercise rationalStrategy)
   { identifier    = "rational"
   , description   = "simplify expression (rational numbers)"
   , finalProperty = (`belongsTo` rationalNormalForm)
   , termGenerator = simpleGenerator (rationalGenerator 5)
   }

fractionExercise :: Exercise Expr
fractionExercise = (numericExercise fractionStrategy)
   { identifier    = "fraction"
   , description   = "simplify expression (fractions)"
   , finalProperty = (`belongsTo` rationalNormalForm)
   , termGenerator = simpleGenerator (rationalGenerator 5)
   }