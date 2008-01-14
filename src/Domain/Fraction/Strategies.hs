-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Strategies where

import Domain.Fraction.Zipper
import Domain.Fraction.Rules
import Common.Strategy

{-
eliminateConstants :: Strategy LogicInContext
eliminateConstants = repeatS $ somewhere $
   altList $ map liftLogicRule rules
 where 
   rules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
           , ruleFalseZeroAnd, ruleNotBoolConst, ruleFalseInEquiv
           , ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
           ]

eliminateImplEquiv :: Strategy LogicInContext
eliminateImplEquiv = repeatS $ somewhere $
          liftLogicRule ruleDefImpl
      <|> liftLogicRule ruleDefEquiv
      
eliminateNots :: Strategy LogicInContext
eliminateNots = repeatS $ somewhere $ 
          liftLogicRule ruleDeMorganAnd
      <|> liftLogicRule ruleDeMorganOr
      <|> liftLogicRule ruleNotNot
      
orToTop :: Strategy LogicInContext
orToTop = repeatS $ somewhere $ liftLogicRule ruleAndOverOr

toDNF :: NamedStrategy LogicInContext
toDNF =  label "Bring to dnf"
      $  label "Eliminate constants"                 eliminateConstants
     <*> label "Eliminate implications/equivalences" eliminateImplEquiv
     <*> label "Eliminate nots"                      eliminateNots 
     <*> label "Move ors to top"                     orToTop
-}