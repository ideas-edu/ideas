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
   ( -- * Power strategies
     simplifyPowerStrategy
   , powerOfStrategy
   , calcPowerStrategy
   , nonNegBrokenExpStrategy
   ) where

import Prelude hiding (repeat, not)

import Common.Classes
import Common.Context
import Common.Navigator
import Common.Strategy
import Domain.Math.Expr
import Domain.Math.Numeric.Rules (divisionNumerator, divisionDenominator)
import Domain.Math.Power.Rules
import Domain.Math.Power.Utils
import Domain.Math.Simplification


-- | Strategies ---------------------------------------------------------------

simplifyPowerStrategy :: LabeledStrategy (Context Expr)
simplifyPowerStrategy = cleanUpStrategyRules "Simplify" powerRules 

powerOfStrategy :: LabeledStrategy (Context Expr)
powerOfStrategy = cleanUpStrategyRules "Write as power of" powerRules 

nonNegBrokenExpStrategy :: LabeledStrategy (Context Expr)
nonNegBrokenExpStrategy = cleanUpStrategy (change cleanup . applyTop cleanup) strategy
  where
    rs = [ addExponents, subExponents, mulExponents, reciprocalInv
         , distributePower, distributePowerDiv, power2root, zeroPower
         , calcPowerPlus, calcPowerMinus
         ]
    strategy = label "Write with non-negative exponent" $ exhaustiveStrategy rs
    cleanup = applyD divisionNumerator
            . applyD myFractionTimes
            . mergeConstants 
            . simplifyWith simplifyConfig {withMergeAlike = False}

calcPowerStrategy :: LabeledStrategy (Context Expr)
calcPowerStrategy = cleanUpStrategy cleanup strategy
  where
    strategy = label "Calculate power" $ exhaustiveStrategy rules
    rules = calcPower : divisionDenominator : reciprocalInv : divBase : rationalRules
    cleanup = applyTop (applyD myFractionTimes)
            . applyD (exhaustiveStrategy $ myFractionTimes: naturalRules)


-- | Rule collections ---------------------------------------------------------

powerRules =
  [ addExponents, subExponents, mulExponents, distributePower, zeroPower
  , reciprocalVar, root2power, calcPower, calcPowerPlus, calcPowerMinus
  , pushNegOut
  ]


-- | Help functions -----------------------------------------------------------

cleanUpStrategyRules l = 
  cleanUpStrategy (change cleanUp. applyTop cleanUp) . label l . exhaustiveStrategy

cleanUp = mergeConstants 
        . simplifyWith simplifyConfig {withMergeAlike = False}
                 