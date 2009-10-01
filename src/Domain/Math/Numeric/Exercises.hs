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
import Domain.Math.Numeric.Generators
import Domain.Math.ExercisesDWO (calculateResults)
import Common.Context
   
------------------------------------------------------------
-- Exercises

numericExercise :: LabeledStrategy Expr -> Exercise Expr
numericExercise s = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
   , equality      = (==)
   , equivalence   = viewEquivalent rationalView
   , ruleset       = rulesInStrategy (liftToContext s)
   , strategy      = liftToContext s
   }

naturalExercise :: Exercise Expr
naturalExercise = (numericExercise naturalStrategy)
   { description   = "simplify expression (natural numbers)"
   , exerciseCode  = makeCode "math" "natural"
   , finalProperty = (`belongsTo` integerNormalForm)
   , termGenerator = ExerciseList (concat calculateResults)
   }

integerExercise :: Exercise Expr
integerExercise = (numericExercise integerStrategy)
   { description   = "simplify expression (integers)"
   , exerciseCode  = makeCode "math" "integer"
   , finalProperty = (`belongsTo` integerNormalForm)
   , termGenerator = ExerciseList (concat calculateResults)
   }
   
rationalExercise :: Exercise Expr
rationalExercise = (numericExercise rationalStrategy)
   { description   = "simplify expression (rational numbers)"
   , exerciseCode  = makeCode "math" "rational"
   , finalProperty = (`belongsTo` rationalNormalForm)
   , termGenerator = simpleGenerator (rationalGenerator 5)
   }

fractionExercise :: Exercise Expr
fractionExercise = (numericExercise fractionStrategy)
   { description   = "simplify expression (fractions)"
   , exerciseCode  = makeCode "math" "fraction"
   , finalProperty = (`belongsTo` rationalNormalForm)
   , termGenerator = simpleGenerator (rationalGenerator 5)
   }