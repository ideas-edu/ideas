-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Logic.Strategies where

import Logic.Zipper
import Logic.Rules
import Strategy

eliminateConstants :: Strategy LogicInContext
eliminateConstants = repeatS $ somewhere $
   altList $ map (toStrategy . logicRuleInContext) rules
 where 
   rules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
           , ruleFalseZeroAnd, ruleNotBoolConst, ruleFalseInEquiv
           , ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
           ]

eliminateImplEquiv :: Strategy LogicInContext
eliminateImplEquiv = repeatS $ somewhere $
          logicRuleInContext ruleDefImpl
      <|> logicRuleInContext ruleDefEquiv
      
eliminateNots :: Strategy LogicInContext
eliminateNots = repeatS $ somewhere $ 
          logicRuleInContext ruleDeMorganAnd
      <|> logicRuleInContext ruleDeMorganOr
      <|> logicRuleInContext ruleNotNot
      
orToTop :: Strategy LogicInContext
orToTop = repeatS $ somewhere $ logicRuleInContext ruleAndOverOr

toDNF :: Strategy LogicInContext
toDNF = eliminateConstants <*> eliminateImplEquiv <*> eliminateNots <*> orToTop