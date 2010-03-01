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
   ( powerStrategy, natView
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
import Domain.Math.Numeric.Rules
import Prelude hiding (repeat)
import Test.QuickCheck hiding (label)

------------------------------------------------------------
-- Strategies

powerStrategy :: LabeledStrategy (Context Expr)
powerStrategy = cleanUpStrategy cleanup $ s powerRules
  where
    cleanup = applyD $ s (calcPower : numericRules)
    s rules = label "simplify" $ repeat $ alternatives $ 
                map (somewhere . liftToContext) rules

-- | Allowed numeric rules
numericRules =
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
   
natView = makeView f fromInteger
  where
    f (Nat n) = Just n
    f _       = Nothing

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