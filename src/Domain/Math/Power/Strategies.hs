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
   ( naturalStrategy, integerStrategy
   , rationalStrategy, fractionStrategy
   , testAll
   ) where

import Common.Apply
import Common.Strategy
import Common.Transformation
import Common.Uniplate
import Common.View
import Domain.Math.Expr
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Generators
import Prelude hiding (repeat)
import Test.QuickCheck hiding (label)

------------------------------------------------------------
-- Strategies

naturalStrategy :: LabeledStrategy Expr
naturalStrategy = label "simplify" $ repeat $ alternatives $ map swRule
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

integerStrategy :: LabeledStrategy Expr
integerStrategy = label "simplify" $ repeat $ alternatives $ map swRule
   [ calcPlusWith     "int" integerNormalForm
   , calcMinusWith    "int" integerNormalForm
   , calcTimesWith    "int" integerNormalForm
   , calcDivisionWith "int" integerNormalForm
   , doubleNegate
   , negateZero
   ]

rationalStrategy :: LabeledStrategy Expr
rationalStrategy = label "simplify" $ repeat $ alternatives $ map swRule
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

fractionStrategy :: LabeledStrategy Expr
fractionStrategy = label "simplify" $ repeat $ alternatives $ map swRule
   [ fractionPlus, fractionPlusScale, fractionTimes
   , calcPlusWith     "int" integerNormalForm
   , calcMinusWith    "int" integerNormalForm
   , calcTimesWith    "int" integerNormalForm -- not needed?
   , calcDivisionWith "int" integerNormalForm
   , doubleNegate
   , negateZero
   , divisionDenominator  
   , divisionNumerator 
   , simplerFraction -- only apply when fractionPlusScale is not applicable
   ]

swRule :: Uniplate a => Rule a -> Rule a
swRule r = makeSimpleRuleList (name r) (somewhereM (applyAll r))

------------------------------------------------------------
-- Test code

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
   
{- testC = quickCheck $ forAll (sized rationalGenerator) $ \e -> 
   let a = cleanUp e
   in a == cleanUp a -}