-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Strategies
   ( simplifyPowerStrategy
   , powerOfStrategy
   , calcPowerStrategy
   , nonNegExpStrategy
   , cleanUp
   ) where

import Prelude hiding (repeat, not)

import Common.Classes
import Common.Context
import Common.Navigator
import Common.Strategy
import Domain.Math.Expr
import Domain.Math.Power.Rules
import Domain.Math.Power.Utils
import Domain.Math.Simplification


-- | Strategies ---------------------------------------------------------------

simplifyPowerStrategy :: LabeledStrategy (Context Expr)
simplifyPowerStrategy = cleanUpStrategyRules "Simplify" powerRules 

powerOfStrategy :: LabeledStrategy (Context Expr)
powerOfStrategy = cleanUpStrategyRules "Write as power of" powerRules 

nonNegExpStrategy :: LabeledStrategy (Context Expr)
nonNegExpStrategy = cleanUpStrategyRules "Write with non-negative exponent" rs
  where
    -- rs = [ addExponents, subExponents, mulExponents, reciprocalInv
    --      , distributePower, distributePowerDiv, power2root, zeroPower
    --      , calcPowerPlus, calcPowerMinus {-, myFractionTimes, reciprocalFrac -}
    --      ] 
    rs = [ reciprocalInv
         ]

calcPowerStrategy :: LabeledStrategy (Context Expr)
calcPowerStrategy = cleanUpStrategyRules "Calculate power" rules
  where
    rules = calcPower : divBase : rationalRules


-- | Rule collections ---------------------------------------------------------

powerRules =
  [ addExponents, subExponents, mulExponents, distributePower, zeroPower
  , reciprocalVar, root2power, calcPower, calcPowerPlus, calcPowerMinus
  , pushNegOut
  ]


-- | Help functions -----------------------------------------------------------

cleanUpStrategyRules l = 
  cleanUpStrategy (change cleanUp) . label l . exhaustiveStrategy

cleanUp = mergeConstants . simplifyWith simplifyConfig {withMergeAlike = False}
                 