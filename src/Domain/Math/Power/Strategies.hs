-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   , calcPowerStrategy
   , nonNegBrokenExpStrategy
   ) where

import Common.Library hiding (simplifyWith)
import Domain.Math.Expr
import Domain.Math.Numeric.Rules (divisionNumerator, divisionDenominator)
import Domain.Math.Power.Rules
import Domain.Math.Power.Utils
import Domain.Math.Simplification

-- Strategies ---------------------------------------------------------------

-- | Simplify an expression containing powers as far as possible
simplifyPowerStrategy :: LabeledStrategy (Context Expr)
simplifyPowerStrategy = cleanUpStrategyRules "Simplify" powerRules

nonNegBrokenExpStrategy :: LabeledStrategy (Context Expr)
nonNegBrokenExpStrategy = cleanUpStrategy (change cleanup . applyTop cleanup) $
   label "Write with non-negative exponent" $ exhaustiveStrategy rs
  where
    rs = [ addExponents, subExponents, mulExponents, reciprocalInv
         , distributePower, distributePowerDiv, power2root, zeroPower
         , calcPowerPlus, calcPowerMinus
         ]
    cleanup = applyD divisionNumerator
            . applyD myFractionTimes
            . mergeConstants
            . simplifyWith simplifyConfig {withMergeAlike = False}

calcPowerStrategy :: LabeledStrategy (Context Expr)
calcPowerStrategy = cleanUpStrategy cleanup $
   label "Calculate power" $ exhaustiveStrategy rules
  where
    rules = calcPower : divisionDenominator : reciprocalInv : divBase : rationalRules
    cleanup = applyTop (applyD myFractionTimes)
            . applyD (exhaustiveStrategy $ myFractionTimes : naturalRules)

-- Rule collections ---------------------------------------------------------

powerRules :: [Rule Expr]
powerRules =
  [ addExponents, subExponents, mulExponents, distributePower, zeroPower
  , reciprocalVar, root2power, calcPower, calcPowerPlus, calcPowerMinus
  , pushNegOut
  ]

-- | Help functions -----------------------------------------------------------

cleanUpStrategyRules :: IsId n => n -> [Rule Expr] -> LabeledStrategy (Context Expr)
cleanUpStrategyRules l =
  cleanUpStrategy (change cleanUp. applyTop cleanUp) . label l . exhaustiveStrategy

cleanUp :: Expr -> Expr
cleanUp = mergeConstants
        . simplifyWith simplifyConfig {withMergeAlike = False}