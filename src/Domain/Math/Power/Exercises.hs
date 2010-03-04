-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Power.Exercises    
   ( simplifyPowerExercise
   , powerOfAExercise 
   , nonNegExpExercise ) where

import Prelude hiding ( (^) )

import Common.Apply 
import Common.Exercise
import Common.Strategy
import Common.View
import Common.Context
import Common.Navigator
import Common.Uniplate
import Common.Transformation
import Common.Derivation (derivations, derivation)
import Domain.Math.Examples.DWO3
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Strategies
import Domain.Math.Power.Strategies
import Domain.Math.Power.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Tests

import Data.Maybe
--import Domain.Math.Power.Generators

------------------------------------------------------------
-- Exercises

powerExercise :: LabeledStrategy (Context Expr) -> Exercise Expr
powerExercise s = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
   , navigation    = navigator                     
--   , equivalence   = viewEquivalent rationalView
   , strategy      = s
   }

simplifyPowerExercise :: Exercise Expr
simplifyPowerExercise = (powerExercise powerStrategy)
   { description  = "simplify expression (powers)"
   , exerciseCode = makeCode "math" "simplifyPower"
--   , isReady      = (`belongsTo` integerNormalForm)
   , examples     = concat simplerPowers
   }

powerOfAExercise :: Exercise Expr
powerOfAExercise = (powerExercise powerStrategy)
   { description  = "write as a power of a"
   , exerciseCode = makeCode "math" "powerOfA"
--   , isReady      = (`belongsTo` integerNormalForm)
   , examples     = concat powersOfA
   }

nonNegExpExercise :: Exercise Expr
nonNegExpExercise = (powerExercise nonNegExpStrategy)
   { description  = "write with a non-negative exponent"
   , exerciseCode = makeCode "math" "nonNegExp"
--   , isReady      = (`belongsTo` integerNormalForm)
   , examples     = concat nonNegExp
   }


-- | test stuff
showDerivations ex exercises level = 
  mapM_ (putStrLn . showDerivation ex) $ exercises !! level
                        
a = Var "a"
b = Var "b"
