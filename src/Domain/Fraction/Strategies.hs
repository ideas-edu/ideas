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

import Prelude hiding (repeat)
import Domain.Fraction.Frac
import Domain.Fraction.Rules
import Common.Context (Context, liftRuleToContext)
import Common.Strategy

toSimple :: LabeledStrategy (Context Frac)
toSimple = label "Simplify expression" $ repeat $
           label "Eliminate zero's" eliminateZeros
       <*> label "Eliminate units"  eliminateUnits
       <*> label "Do calculation"   calculate

eliminateZeros :: Strategy (Context Frac)
eliminateZeros = repeat $ somewhere $
                 liftRuleToContext ruleDivZero
             <|> liftRuleToContext ruleMulZero
             <|> liftRuleToContext ruleUnitAdd
             <|> liftRuleToContext ruleSubZero

eliminateUnits :: Strategy (Context Frac)
eliminateUnits = repeat $ somewhere $ 
                 liftRuleToContext ruleUnitMul
             <|> liftRuleToContext ruleDivOne
             <|> liftRuleToContext ruleDivSame
             <|> liftRuleToContext ruleMulVar
             <|> liftRuleToContext ruleSubVar

calculate :: Strategy (Context Frac)
calculate = somewhere $
            liftRuleToContext ruleMul
        <|> liftRuleToContext ruleDiv
        <|> liftRuleToContext ruleAdd
        <|> liftRuleToContext ruleSub
        <|> liftRuleToContext ruleGCD
        <|> liftRuleToContext ruleDistMul
        <|> liftRuleToContext ruleCommonDenom <*> liftRuleToContext ruleAddFrac
        <|> liftRuleToContext ruleCommonDenom <*> liftRuleToContext ruleSubFrac
--       <|> calcFrac


{-
calcFrac :: Strategy FracInContext
calcFrac =  liftRuleToContext ruleCommonDenom 
        <*> (liftRuleToContext ruleAddFrac <|> liftRuleToContext ruleSubFrac)
        <*> liftRuleToContext ruleGCD
-}