-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.Strategies 
   ( linearStrategy, linearMixedStrategy
   , quadraticStrategy, quadraticStrategyG
   , higherDegreeStrategy, higherDegreeStrategyG
   , findFactorsStrategy, findFactorsStrategyG
   ) where

import Prelude hiding (repeat, replicate, fail)
import Common.Strategy
import Common.Navigator
import Common.Transformation
import Common.Id
import Common.Uniplate (transform)
import Common.View
import Common.Context
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Polynomial.Rules
import Domain.Math.Numeric.Views
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Polynomial.CleanUp
import Data.Maybe
import Common.Rewriting.Term

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Context (Equation Expr))
linearStrategy = cleanUpStrategy (applyTop (fmap cleanUpSimple)) linearStrategyG

linearMixedStrategy :: LabeledStrategy (Context (Equation Expr))
linearMixedStrategy = 
   let f   = applyTop (fmap (transform (simplify mixedFractionView) . cleanUpSimple))
       cfg = [ (ByName (showId ruleNormalizeMixedFraction), Reinsert)
             , (ByName (showId ruleNormalizeRational), Remove)
             ] 
   in cleanUpStrategy f (configureNow (configure cfg linearStrategyG))

linearStrategyG :: IsTerm a => LabeledStrategy (Context a)
linearStrategyG =
   label "Linear Equation" $
       label "Phase 1" (repeat (
               use removeDivision
          <|>  multi (showId distributeTimes) (somewhere (useC parentNotNegCheck <*> use distributeTimes))
          <|>  multi "merge similar terms" (once (use merge))))
   <*> label "Phase 2" (repeat (
              (use flipEquation |> use varToLeft)
          <|> use (coverUpPlusWith oneVar) 
          <|> use (coverUpMinusLeftWith oneVar)
          <|> use (coverUpMinusRightWith oneVar)
          <|> use coverUpTimes 
          <|> use coverUpNegate
           ))
   <*> repeat (once 
          (  use ruleNormalizeRational
         <|> remove (use ruleNormalizeMixedFraction)
          ))
   
------------------------------------------------------------
-- Quadratic equations

quadraticStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
quadraticStrategy = 
   cleanUpStrategy (applyTop cleanUpRelation) quadraticStrategyG

quadraticStrategyG :: IsTerm a => LabeledStrategy (Context a)
quadraticStrategyG = 
   label "Quadratic Equation Strategy" $ repeat $
   -- Relaxed strategy: even if there are "nice" factors, allow use of quadratic formula
      somewhere (generalForm <|> generalABCForm)
      |> somewhere zeroForm 
      |> somewhere constantForm
      |> simplifyForm
      |> topForm 
 where
   -- ax^2 + bx + c == 0, without quadratic formula
   generalForm = label "general form" $ 
          use commonFactorVar
      <|> use noLinFormula
      <|> use simplerPolynomial
      <|> remove (use bringAToOne)
      <|> use niceFactors
      <|> use coverUpPower -- to deal with special case x^2=0
            
   generalABCForm = label "abc form" $ 
      useC abcFormula
      
   zeroForm = label "zero form" $
      use mulZero
    
   -- expr == c
   constantForm = label "constant form" $
          use (coverUpPlusWith oneVar)
      <|> use (coverUpMinusLeftWith oneVar)
      <|> use (coverUpMinusRightWith oneVar)
      <|> use coverUpTimes
      <|> use coverUpNegate
      <|> use coverUpNumerator
      <|> use squareBothSides 
      <|> use factorLeftAsSquare
      
   -- simplifies square roots, or do an approximation 
   simplifyForm =
      label "square root simplification" (
         multi (showId simplerSquareRoot) (somewhere (use simplerSquareRoot)))
      <|> 
      remove (label "approximate result" (
         multi (showId ruleApproximate) (somewhere (use ruleApproximate))))

   topForm = label "top form" $
        somewhere (use cancelTerms  <|> use sameFactor)
      |> (  somewhere (use sameConFactor)
        <|> multi (showId merge) (somewhere (use merge))
        <|> somewhere (use distributionSquare)
        <|> multi (showId distributeTimes) (somewhere 
               (useC parentNotNegCheck <*> use distributeTimes))
        <|> multi (showId distributeDivision) (somewhere 
               (once (use distributeDivision)))
        <|> somewhere (use flipEquation)
         )
      |> somewhere (use moveToLeft <|> remove (use prepareSplitSquare))

-----------------------------------------------------------
-- Higher degree equations

higherDegreeStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
higherDegreeStrategy = 
   cleanUpStrategy (applyTop cleanUpRelation) higherDegreeStrategyG

higherDegreeStrategyG :: IsTerm a => LabeledStrategy (Context a)
higherDegreeStrategyG = label "higher degree" $ 
   higherForm 
   <*> label "quadratic"  quadraticStrategyG
   <*> afterSubst
 where
   higherForm = label "higher degree form" $ repeat $
      somewhere (use allPowerFactors)
      |> somewhere (
              use coverUpPower
          <|> use mulZero
          <|> use sameFactor
          <|> use coverUpTimes
          <|> use exposeSameFactor
          <|> use (coverUpPlusWith oneVar)
          <|> use (coverUpMinusLeftWith oneVar)
          <|> use (coverUpMinusRightWith oneVar)
          <|> use sameConFactor
          <|> useC higherSubst)
      |> somewhere (use moveToLeft)
   
   afterSubst = label "afterwards" $ try $
      useC substBackVar  <*> repeat (somewhere (use coverUpPower)) 

-----------------------------------------------------------
-- Finding factors in an expression

findFactorsStrategy :: LabeledStrategy (Context Expr)
findFactorsStrategy = cleanUpStrategy (applyTop cleanUpSimple) $
   label "find factors" $ replicate 10 $ try $ findFactorsStrategyG
   
findFactorsStrategyG :: IsTerm a => LabeledStrategy (Context a)
findFactorsStrategyG = label "find factor step" $
   somewhereTimes $ 
      use niceFactorsNew <|> use commonFactorVarNew 
      <|> use factorVariablePower <|> use simplerLinearFactor

somewhereTimes :: IsStrategy f => f (Context a) -> Strategy (Context a)
somewhereTimes s = fix $ \this -> s <|> once this
 where
   once s = ruleMoveDown <*> s <*> ruleMoveUp
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDownTimes" $ \c ->
      if (isTimesC c) then allDowns c else []
   ruleMoveUp   = minorRule $ makeSimpleRule "MoveUp" safeUp
   safeUp a     = maybe (Just a) Just (up a)
   
isTimesC :: Context a -> Bool
isTimesC = maybe False (isJust . isTimes :: Term -> Bool) . currentT