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
        <*> label "Eliminate units" eliminateUnits
        <*> label "Do calculation" calculate

eliminateZeros :: Strategy FracInContext
eliminateZeros = repeatS $ somewhere $
                 liftFracRule ruleDivZero
             <|> liftFracRule ruleMulZero
             <|> liftFracRule ruleUnitAdd
             <|> liftFracRule ruleSubZero


eliminateUnits :: Strategy FracInContext
eliminateUnits = repeatS $ somewhere $ 
                 liftFracRule ruleUnitMul
             <|> liftFracRule ruleDivOne
             <|> liftFracRule ruleDistMul
             <|> liftFracRule ruleDivSame
             <|> liftFracRule ruleDivReciprocal
             <|> liftFracRule ruleMulVar
             <|> liftFracRule ruleSubVar


calculate :: Strategy FracInContext
calculate = repeatS $ somewhere $
            liftFracRule ruleDiv
        <|> liftFracRule ruleMul
        <|> liftFracRule ruleCommonDenom <*> liftFracRule ruleAdd
        <|> liftFracRule ruleCommonDenom <*> liftFracRule ruleSub
