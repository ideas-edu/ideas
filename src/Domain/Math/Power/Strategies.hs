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
module Domain.Math.Power.Strategies
   ( powerStrategy, natView, nonNegExpStrategy, hasNegExp
   ) where

import Common.Apply
import Common.Context
import Common.Strategy
import Common.Transformation
import Common.Uniplate hiding (somewhere)
import Common.View
import Domain.Math.Expr
import Domain.Math.Power.Rules
import Domain.Math.Power.Views
import Domain.Math.Numeric.Generators
import Domain.Math.Numeric.Strategies
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
import Prelude hiding (repeat)
import Test.QuickCheck hiding (label)

------------------------------------------------------------
-- Strategies

makeStrategy :: String -> [Rule Expr] -> [Rule Expr] -> LabeledStrategy (Context Expr)
makeStrategy l rs cs = cleanUpStrategy (cleanup cs) $ strategy rs
  where
    cleanup  = applyD . strategy
    strategy = label l . repeat . alternatives . map (somewhere . liftToContext)
                

powerStrategy :: LabeledStrategy (Context Expr)
powerStrategy = makeStrategy 
  "simplify" powerRules (calcPower : naturalRules ++ rationalRules)
  where
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
      ]

nonNegExpStrategy :: LabeledStrategy (Context Expr)
nonNegExpStrategy = makeStrategy "non negative exponent" rules cleanupRules
  where
    rules = [ addExponents
            , subExponents
            , mulExponents
            , reciprocal' hasNegExp
            , distributePower
            , power2root
            , distributeRoot
            , zeroPower
            , calcPowerPlus
            , calcPowerMinus
            , myFractionTimes
            ] ++ fractionRules            
    cleanupRules = calcPower : simplifyFraction : naturalRules
    
hasNegExp expr = 
  case match unitPowerView expr of
    Just (_, (_, x)) -> x < 0
    _ -> False    


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

------------------------------------------------------------
-- Test code

{-
testAll :: IO ()
testAll = sequence_ [test1, test2, test3, test4]

test1 = quickCheck $ forAll (sized integerGenerator) $ \e -> 
   Prelude.not (e `belongsTo` integerView) || 
   applyD naturalStrategy e `belongsTo` integerNormalForm
   
test2 = quickCheck $ forAll (sized integerGenerator) $ \e -> 
   Prelude.not (e `belongsTo` integerView) || 
   applyD integerStrategy e `belongsTo` integerNormalForm
   
test3 = quickCheck $ forAll (sized rationalGenerator) $ \e -> 
   Prelude.not (e `belongsTo` rationalView) || 
   applyD rationalStrategy e `belongsTo` rationalNormalForm
   
test4 = quickCheck $ forAll (sized rationalGenerator) $ \e -> 
   Prelude.not (e `belongsTo` rationalView) || 
   applyD fractionStrategy e `belongsTo` rationalNormalForm
-}
 
{- testC = quickCheck $ forAll (sized rationalGenerator) $ \e -> 
   let a = cleanUp e
   in a == cleanUp a -}