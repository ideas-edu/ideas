-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic.Strategies where

import Prelude hiding (repeat)
import Domain.Logic.Rules
import Domain.Logic.Formula
import Common.Context (Context, liftRuleToContext)
import Common.Strategy

eliminateConstants :: Strategy (Context Logic)
eliminateConstants = repeat $ somewhere $
   alternatives $ map liftRuleToContext rules
 where 
   rules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
           , ruleFalseZeroAnd, ruleNotBoolConst, ruleFalseInEquiv
           , ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
           ]
	   
eliminateConstantsDWA :: Strategy (Context Logic)
eliminateConstantsDWA = repeat $ somewhere $
   alternatives $ map liftRuleToContext rules
 where 
   rules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
           , ruleFalseZeroAnd, ruleNotBoolConst
           ]

eliminateImplEquiv :: Strategy (Context Logic)
eliminateImplEquiv = repeat $ somewhere $
          liftRuleToContext ruleDefImpl
      <|> liftRuleToContext ruleDefEquiv
      
eliminateNots :: Strategy (Context Logic)
eliminateNots = repeat $ somewhere $ 
          liftRuleToContext ruleDeMorganAnd
      <|> liftRuleToContext ruleDeMorganOr
      <|> liftRuleToContext ruleNotNot
      
orToTop :: Strategy (Context Logic)
orToTop = repeat $ somewhere $ liftRuleToContext ruleAndOverOr

toDNF :: LabeledStrategy (Context Logic)
toDNF =  label "Bring to dnf"
      $  label "Eliminate constants"                 eliminateConstants
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop