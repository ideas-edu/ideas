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

import Domain.Fraction.Zipper
import Domain.Fraction.Rules
import Common.Strategy

toSimple :: NamedStrategy FracInContext
toSimple =  label "Simplify expression"
         $  label "Eliminate zero's" eliminateZeros
        <*> label "Eliminate divsions and mulitplications" eliminateDivMul
        <*> label "Find common denominator and do addition/substraction" eliminateAddSub

eliminateZeros :: Strategy FracInContext
eliminateZeros = repeatS $ somewhere $
                 liftFracRule ruleDivZero
             <|> liftFracRule ruleMulZero
             <|> liftFracRule ruleUnitAdd
             <|> liftFracRule ruleSubZero


eliminateDivMul :: Strategy FracInContext
eliminateDivMul = repeatS $ somewhere $ 
                  liftFracRule ruleDivZero
              <|> liftFracRule ruleUnitMul
              <|> liftFracRule ruleDiv
              <|> liftFracRule ruleMul
              <|> liftFracRule ruleDistMul
              <|> liftFracRule ruleDivReciprocal

eliminateAddSub :: Strategy FracInContext
eliminateAddSub = repeatS $ somewhere $
                  liftFracRule ruleAdd
              <|> liftFracRule ruleSub
              <|> liftFracRule ruleCommonDenom -- to get same denominator
