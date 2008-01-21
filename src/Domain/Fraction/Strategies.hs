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
toSimple = label "Simplify expression" $ repeatNS $
           label "Eliminate zero's" eliminateZeros
       <*> label "Eliminate units"  eliminateUnits
       <*> label "Do calculation"   calculate

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
             <|> liftFracRule ruleDivSame
             <|> liftFracRule ruleMulVar
             <|> liftFracRule ruleSubVar

calculate :: Strategy FracInContext
calculate = somewhere $
            liftFracRule ruleMul
--        <|> liftFracRule ruleDivReciprocal
        <|> liftFracRule ruleDiv
        <|> liftFracRule ruleAdd
        <|> liftFracRule ruleCommonDenom <*> liftFracRule ruleAddFrac
        <|> liftFracRule ruleSubVar
        <|> liftFracRule ruleSub
        <|> liftFracRule ruleCommonDenom <*> liftFracRule ruleSubFrac
