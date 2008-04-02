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
eliminateConstantsDWA = somewhere $
   alternatives $ map liftRuleToContext rules
 where 
   rules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
           , ruleFalseZeroAnd, ruleNotBoolConst
           ]

simplifyDWA :: Strategy (Context Logic)
simplifyDWA = somewhere $
   	  liftRuleToContext ruleNotNot
      <|> liftRuleToContext ruleIdempOr
      <|> liftRuleToContext ruleIdempOr
      <|> liftRuleToContext ruleAbsorpOr
      <|> liftRuleToContext ruleAbsorpOr

eliminateImplEquiv :: Strategy (Context Logic)
eliminateImplEquiv = repeat $ somewhere $
          liftRuleToContext ruleDefImpl
      <|> liftRuleToContext ruleDefEquiv

eliminateImplEquivDWA :: Strategy (Context Logic)
eliminateImplEquivDWA = somewhere $
          liftRuleToContext ruleDefImpl
      <|> liftRuleToContext ruleDefEquiv
      
eliminateNots :: Strategy (Context Logic)
eliminateNots = repeat $ somewhere $ 
          liftRuleToContext ruleDeMorganAnd
      <|> liftRuleToContext ruleDeMorganOr
      <|> liftRuleToContext ruleNotNot

eliminateNotsDWA :: Strategy (Context Logic)
eliminateNotsDWA = somewhere $ 
          liftRuleToContext ruleDeMorganAnd
      <|> liftRuleToContext ruleDeMorganOr
      
orToTop :: Strategy (Context Logic)
orToTop = repeat $ somewhere $ liftRuleToContext ruleAndOverOr

orToTopDWA :: Strategy (Context Logic)
orToTopDWA = somewhere $ liftRuleToContext ruleAndOverOr

toDNF :: LabeledStrategy (Context Logic)
toDNF =  label "Bring to dnf"
      $  label "Eliminate constants"                 eliminateConstants
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop
     
toDNFDWA :: LabeledStrategy (Context Logic)
toDNFDWA =  label "Bring to dnf" $ repeat $
      label "Simplify"                            (eliminateConstantsDWA <|> simplifyDWA)
   |> label "Eliminate implications/equivalences" eliminateImplEquivDWA
   |> label "Eliminate nots"                      eliminateNotsDWA
   |> label "Move ors to top"                     orToTopDWA
{-
      $  label "Eliminate constants"                 eliminateConstantsDWA
     <*> label "Simplify"		    	     simplifyDWA
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop -}