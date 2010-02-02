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
   ( simplifyPowerExercise ) where

import Prelude hiding ( (^) )

import Common.Apply 
import Common.Exercise
import Common.Strategy
import Common.View
import Common.Context
import Common.Uniplate
import Domain.Math.Examples.DWO3
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Rules
import Domain.Math.Power.Strategies
import Domain.Math.Power.Views
import Domain.Math.Power.Rules
--import Domain.Math.Power.Generators


Just (s, es) = match productView   $ 2*Var "a"^4 * 3*Var "a"^3* 5

------------------------------------------------------------
-- Exercises

powerExercise :: LabeledStrategy Expr -> Exercise Expr
powerExercise s = makeExercise 
   { status        = Provisional
   , parser        = parseExpr
--   , equivalence   = viewEquivalent rationalView
   , strategy      = mapRules liftToContext s
   }

simplifyPowerExercise :: Exercise Expr
simplifyPowerExercise = (powerExercise (label "" succeed))
   { description  = "simplify expression (powers)"
   , exerciseCode = makeCode "math" "simplifyPower"
--   , isReady      = (`belongsTo` integerNormalForm)
   , examples     = concat simplerPowers
   }
