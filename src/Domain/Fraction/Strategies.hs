-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)e
--
-----------------------------------------------------------------------------
module Domain.Fraction.Strategies where

import Prelude hiding (repeat, fail)
import Domain.Fraction.Frac
import Domain.Fraction.Rules
import Common.Context (Context, liftRuleToContext)
import Common.Strategy
import Common.Transformation
import Common.Context
import Common.Apply
import Domain.Fraction.Parser


foldComb comb unit rs = foldl comb unit $ map liftRuleToContext rs

toSimple :: LabeledStrategy (Context Frac)
toSimple = label "All rules" $ repeat $ zero <*> unit <*> calc
  where
    zero = label "Eliminate zeros" $ repeat $ somewhere $ foldComb (<|>) fail zeroRules
    unit = label "Eliminate units" $ repeat $ somewhere $ foldComb (<|>) fail unitRules
    calc = label "Do calculation"  $ somewhere $ (foldComb (<|>) fail calcRules <|> calcFrac')

altSimple = label "All rules" $ repeat $ somewhere $ foldComb (<|>) fail $ zeroRules ++ unitRules ++ calcRules ++ gcdRules

zeroRules :: [FracRule]
zeroRules = [ruleDivZero, ruleMulZero, ruleUnitAdd, ruleSubZero]

unitRules :: [FracRule]
unitRules = [ruleUnitMul, ruleDivOne, ruleDivSame, ruleMulVar, ruleSubVar, ruleNeg]

calcRules :: [FracRule]
calcRules = [ruleMul, ruleDiv, ruleAdd, ruleSub, ruleGCD, ruleDistMul]

calcFrac :: Strategy (Context Frac)
calcFrac =  liftRuleToContext ruleCommonDenom 
        <*> (liftRuleToContext ruleAddFrac <|> liftRuleToContext ruleSubFrac)
        <*> liftRuleToContext ruleGCD

gcdRules = [ruleCommonDenom, ruleAddFrac, ruleSubFrac, ruleGCD]

calcFrac' =  liftRuleToContext ruleCommonDenom <*> liftRuleToContext ruleAddFrac
         <|> liftRuleToContext ruleCommonDenom <*> liftRuleToContext ruleSubFrac

