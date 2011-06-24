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
   
import Common.Library
import Domain.Math.Expr
import Domain.Math.Numeric.Strategies
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Generators
   
------------------------------------------------------------
-- Exercises

numericExercise :: LabeledStrategy (Context Expr) -> Exercise Expr
numericExercise s = makeExercise 
   { status       = Alpha
   , parser       = parseExpr
   , equivalence  = withoutContext (viewEquivalent rationalView)
   , strategy     = s
   , navigation   = termNavigator
   }

naturalExercise :: Exercise Expr
naturalExercise = (numericExercise naturalStrategy)
   { exerciseId   = describe "simplify expression (natural numbers)" $ 
                       newId "numbers.natural"
   , ready        = predicateView integerNF
   , examples     = level Medium $ concat calculateResults
   }

integerExercise :: Exercise Expr
integerExercise = (numericExercise integerStrategy)
   { exerciseId   = describe "simplify expression (integers)" $ 
                       newId "numbers.integers"
   , ready        = predicateView integerNF
   , examples     = level Medium $ concat calculateResults
   }
   
rationalExercise :: Exercise Expr
rationalExercise = (numericExercise rationalStrategy)
   { exerciseId     = describe "simplify expression (rational numbers)" $ 
                         newId "numbers.rational"
   , ready          = predicateView rationalNormalForm
   , randomExercise = simpleGenerator (rationalGenerator 5)
   }

fractionExercise :: Exercise Expr
fractionExercise = (numericExercise fractionStrategy)
   { exerciseId     = describe "simplify expression (fractions)" $ 
                         newId "arithmetic.fractions"
   , ready          = predicateView rationalNormalForm
   , randomExercise = simpleGenerator (rationalGenerator 5)
   }
   
calculateResults :: [[Expr]]
calculateResults = [level1, level2, level3]
 where
   level1 = 
      [ -8*(-3), -3-9, 55/(-5), -6*9, -11- (-3), 6-(-9), -10+3, 6+(-5) ]
   level2 = 
      [ -3-(6*(-3)), -12/3 - 3, -4*(2+3), 2-6*6
      , -27/(4-(-5)), (-24/(-6)) - 3, 8-(-77/(-11)), 4/(-4+5)
      ]
   level3 = 
      [ 4*(3-(6-2)), (-16-9)/5 - 3, 4- (4-13)/(-3), (3*(-3))-5-4
      , -55/(3*(-5)+4), -4*(-2+ (-4)+7), -8 - (140/4*5), (13-(2-1)) / 3
      ]