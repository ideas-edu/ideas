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
module Domain.Math.Numeric.Exercises    
   ( naturalExercise, integerExercise
   , rationalExercise, fractionExercise
   ) where
   
import Common.Exercise
import Common.Strategy
import Common.View
import Domain.Math.Expr
import Domain.Math.Numeric.Strategies
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Generators
import Domain.Math.Examples.DWO1 (calculateResults)
import Common.Context
   
------------------------------------------------------------
-- Exercises

numericExercise :: LabeledStrategy Expr -> Exercise Expr
numericExercise s = makeExercise 
   { status        = Alpha
   , parser        = parseExpr
   , equivalence   = viewEquivalent rationalView
   , strategy      = mapRules liftToContext s
   }

naturalExercise :: Exercise Expr
naturalExercise = (numericExercise naturalStrategy)
   { exerciseCode = describe "simplify expression (natural numbers)" $ 
                       makeCode "math" "natural"
   , isReady      = (`belongsTo` integerNormalForm)
   , examples     = concat calculateResults
   }

integerExercise :: Exercise Expr
integerExercise = (numericExercise integerStrategy)
   { exerciseCode = describe "simplify expression (integers)" $ 
                       makeCode "math" "integer"
   , isReady      = (`belongsTo` integerNormalForm)
   , examples     = concat calculateResults
   }
   
rationalExercise :: Exercise Expr
rationalExercise = (numericExercise rationalStrategy)
   { exerciseCode   = describe "simplify expression (rational numbers)" $ 
                         makeCode "math" "rational"
   , isReady        = (`belongsTo` rationalNormalForm)
   , randomExercise = simpleGenerator (rationalGenerator 5)
   }

fractionExercise :: Exercise Expr
fractionExercise = (numericExercise fractionStrategy)
   { exerciseCode   = describe "simplify expression (fractions)" $ 
                         makeCode "math" "fraction"
   , isReady        = (`belongsTo` rationalNormalForm)
   , randomExercise = simpleGenerator (rationalGenerator 5)
   }