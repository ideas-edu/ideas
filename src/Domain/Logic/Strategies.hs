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
import Domain.Logic.Zipper
import Domain.Logic.Rules
import Common.Strategy

eliminateConstants :: Strategy LogicInContext
eliminateConstants = repeat $ somewhere $
   alternatives $ map liftLogicRule rules
 where 
   rules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
           , ruleFalseZeroAnd, ruleNotBoolConst, ruleFalseInEquiv
           , ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
           ]

eliminateImplEquiv :: Strategy LogicInContext
eliminateImplEquiv = repeat $ somewhere $
          liftLogicRule ruleDefImpl
      <|> liftLogicRule ruleDefEquiv
      
eliminateNots :: Strategy LogicInContext
eliminateNots = repeat $ somewhere $ 
          liftLogicRule ruleDeMorganAnd
      <|> liftLogicRule ruleDeMorganOr
      <|> liftLogicRule ruleNotNot
      
orToTop :: Strategy LogicInContext
orToTop = repeat $ somewhere $ liftLogicRule ruleAndOverOr

toDNF :: LabeledStrategy LogicInContext
toDNF =  label "Bring to dnf"
      $  label "Eliminate constants"                 eliminateConstants
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop