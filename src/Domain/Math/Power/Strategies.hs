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
nonNegExpStrategy = cleanUpStrategy cleanup strategy
  where
    strategy = label "Write with non-negative exponent" $ exhaustiveStrategy $
      [ addExponents, subExponents, mulExponents, reciprocalInv
      , distributePower, distributePowerDiv, power2root, zeroPower
      , calcPowerPlus, calcPowerMinus, myFractionTimes, reciprocalFrac
      ] ++ fractionRules
    cleanup = applyD (exhaustiveStrategy cleanupRules) 
            . applyD (repeat $ somewhere $ simpilfyFrac)
    cleanupRules = calcPower : naturalRules
    simpilfyFrac = (liftToContext simplifyFraction) 
                <*> not (somewhere $ liftToContext myFractionTimes)
  
calcPowerStrategy :: LabeledStrategy (Context Expr)
calcPowerStrategy = cleanUpStrategyRules "Calculate power" rules
  --cleanUpStrategyRules cleanupRules strategy
  where
    strategy = label "Calculate power" $ exhaustiveStrategy rules
    rules = calcPower : divBase : rationalRules
    cleanupRules = rationalRules ++ naturalRules


-- | Rule collections ---------------------------------------------------------

powerRules =
  [ addExponents, subExponents, mulExponents, distributePower, zeroPower
  , reciprocalVar, root2power, calcPower, calcPowerPlus, calcPowerMinus
  , pushNegOut
  ]


-- | Help functions -----------------------------------------------------------

cleanUpStrategyRules l = cleanUpStrategy cleanUp . label l . exhaustiveStrategy

cleanUp = change ( mergeConstants
                 . simplifyWith simplifyConfig {withMergeAlike = False} )