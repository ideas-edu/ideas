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
{-   ( powerStrategy
   , powerOfStrategy
   , calcPowerStrategy
   , nonNegExpStrategy
   ) -}where

import Common.Classes
import Common.Context
import Common.Strategy
import Common.Transformation
import Common.View
import Domain.Math.Expr
import Domain.Math.Power.Rules
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
import Prelude hiding (repeat, not)

------------------------------------------------------------
-- Strategies

powerStrategy :: LabeledStrategy (Context Expr)
powerStrategy = makeStrategy "simplify" rules cleanupRules
  where 
    rules = powerRules 
    cleanupRules = calcPower : naturalRules ++ rationalRules

powerOfStrategy :: LabeledStrategy (Context Expr)
powerOfStrategy = makeStrategy "write as power of" rules cleanupRules
  where
   rules = powerRules 
   cleanupRules = calcPower 
                : simplifyRoot 
                : simplifyFraction 
                : naturalRules 
               ++ rationalRules

nonNegExpStrategy :: LabeledStrategy (Context Expr)
nonNegExpStrategy = cleanUpStrategy cleanup $ strategise "non neg exponent" rules
  where
    rules = [ addExponents
            , subExponents
            , mulExponents
            , reciprocalInv hasNegExp
            , distributePower
            , distributePowerDiv
            , power2root
            , distributeRoot
            , zeroPower
            , calcPowerPlus
            , calcPowerMinus
            , myFractionTimes
            , reciprocalFrac
            ] ++ fractionRules
    cleanup = applyD $ repeat $ alternatives $
                simp : (map (somewhere . liftToContext) $ calcPower : naturalRules)
    simp = (liftToContext simplifyFraction) <*> not (somewhere $ liftToContext myFractionTimes)

calcPowerStrategy :: LabeledStrategy (Context Expr)
calcPowerStrategy = makeStrategy "calcPower" rules cleanupRules
  where
    rules = calcPower 
          : mulRootCom
          : divRoot 
          : rationalRules
    cleanupRules = rationalRules ++ naturalRules

------------------------------------------------------------
-- | Help functions

makeStrategy :: String -> [Rule Expr] -> [Rule Expr] -> LabeledStrategy (Context Expr)
makeStrategy l rs cs = cleanUpStrategy f $ strategise l rs
  where
    f = applyD $ strategise l cs

strategise l = label l . repeat . alternatives . map (somewhere . liftToContext)
--    strategise l = label l . Common.Strategy.replicate 100 . try . alternatives . map (somewhere . liftToContext)

powerRules =
      [ addExponents
      , subExponents
      , mulExponents
      , distributePower
      , zeroPower
      , reciprocal
      , root2power
      , distributeRoot
      , calcPower
      , calcPowerPlus
      , calcPowerMinus
      , myFractionTimes
      , pushNegOut
      ]

-- | Allowed numeric rules
naturalRules =
   [ calcPlusWith     "nat" natView
   , calcMinusWith    "nat" natView
   , calcTimesWith    "nat" natView
   , calcDivisionWith "nat" natView
   , doubleNegate
   , negateZero
   , plusNegateLeft
   , plusNegateRight
   , minusNegateLeft
   , minusNegateRight
   , timesNegateLeft
   , timesNegateRight   
   , divisionNegateLeft
   , divisionNegateRight  
   ]
   where
     natView = makeView f fromInteger
       where
         f (Nat n) = Just n
         f _       = Nothing
 
rationalRules =    
   [ calcPlusWith     "rational" rationalRelaxedForm
   , calcMinusWith    "rational" rationalRelaxedForm
   , calcTimesWith    "rational" rationalRelaxedForm
   , calcDivisionWith "int"      integerNormalForm
   , doubleNegate
   , negateZero
   , divisionDenominator
   , divisionNumerator
   , simplerFraction
   ]
   
fractionRules =
   [ fractionPlus, fractionPlusScale, fractionTimes
   , calcPlusWith     "int" integerNormalForm
   , calcMinusWith    "int" integerNormalForm
   , calcTimesWith    "int" integerNormalForm -- not needed?
   , calcDivisionWith "int" integerNormalForm
   , doubleNegate
   , negateZero
   , smartRule divisionDenominator  
   , smartRule divisionNumerator 
   , simplerFraction
   ]
