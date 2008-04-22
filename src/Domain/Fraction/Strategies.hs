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

import Prelude hiding (repeat)
import Domain.Fraction.Frac
import Domain.Fraction.Rules
import Common.Context (Context, liftRuleToContext)
import Common.Strategy

import Common.Context
import Common.Apply
import Domain.Fraction.Parser
q = inContext $ fst $ parseFrac "(((-9 + -40) / (2 + 5)) / ((-13 * 7) + (-31 * -8)))"

Just q1 = apply toSimple q
Just q2 = apply toSimple q1
Just q3 = apply toSimple q2

w = apply calculate q3

Just q4 = apply toSimple q3
Just q5 = apply toSimple q4
Just q6 = apply toSimple q5
Just q7 = apply toSimple q6


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
             <|> liftRuleToContext ruleNeg

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