-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( linearStrategy, linearMixedStrategy, linearStrategyG
   , quadraticStrategy, quadraticStrategyG
   , higherDegreeStrategy, higherDegreeStrategyG
   , findFactorsStrategy, findFactorsStrategyG, expandStrategy
   ) where

import Common.Library
import Common.Utils.Uniplate (transform)
import Data.Maybe
import Domain.Math.CleanUp
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Views

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Context (Equation Expr))
linearStrategy = cleanUpStrategyAfter (applyTop (fmap cleanUpSimple)) linearStrategyG

linearMixedStrategy :: LabeledStrategy (Context (Equation Expr))
linearMixedStrategy =
   let f   = applyTop (fmap (transform (simplify mixedFractionView) . cleanUpSimple))
       cfg = [ (byName ruleNormalizeMixedFraction, Reinsert)
             , (byName ruleNormalizeRational, Remove)
             ]
   in cleanUpStrategyAfter f (configureNow (configure cfg linearStrategyG))

linearStrategyG :: IsTerm a => LabeledStrategy (Context a)
linearStrategyG =
   label "Linear Equation" $
       label "Phase 1" (repeatS (
               use removeDivision
          <|>  multi (showId distributeTimes) (oncetd (use distributeTimes))
          <|>  multi (showId merge) (layer [] (use merge))))
   <*> label "Phase 2" (repeatS (
              (flipEquationS |> use varToLeft)
          <|> use (coverUpPlusWith oneVar)
          <|> use (coverUpMinusLeftWith oneVar)
          <|> use (coverUpMinusRightWith oneVar)
          <|> use coverUpTimes
          <|> use coverUpNegate
           ))
   <*> repeatS (layer []
          (  use ruleNormalizeRational
         <|> remove (use ruleNormalizeMixedFraction)
          ))

------------------------------------------------------------
-- Quadratic equations

quadraticStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
quadraticStrategy =
   cleanUpStrategyAfter (applyTop cleanUpRelations) quadraticStrategyG

quadraticStrategyG :: IsTerm a => LabeledStrategy (Context a)
quadraticStrategyG =
   label "Quadratic Equation Strategy" $ repeatS $
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
      label "square root simplification" simplerSquareRootMulti
      <|>
      remove (label "approximate result" (
         multi (showId ruleApproximate) (somewhere (use ruleApproximate))))

   topForm = label "top form" $
        somewhere (use cancelTerms  <|> use sameFactor)
      |> (  somewhere (use sameConFactor)
        <|> multi (showId merge) (somewhere (use merge))
        <|> somewhere (use distributionSquare)
        <|> multi (showId distributeTimes) (oncetd
               (use distributeTimes))
        <|> distributeDivisionMulti
        <|> somewhere flipEquationS
         )
      |> somewhere (use moveToLeft <|> remove (use prepareSplitSquare))

-----------------------------------------------------------
-- Higher degree equations

higherDegreeStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
higherDegreeStrategy =
   cleanUpStrategyAfter (applyTop cleanUpRelations) higherDegreeStrategyG

higherDegreeStrategyG :: IsTerm a => LabeledStrategy (Context a)
higherDegreeStrategyG = label "higher degree" $
   higherForm
   <*> label "quadratic"  quadraticStrategyG
   <*> afterSubst
 where
   higherForm = label "higher degree form" $ repeatS $
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
      useC substBackVar  <*> repeatS (somewhere (use coverUpPower))

-----------------------------------------------------------
-- Finding factors in an expression

findFactorsStrategy :: LabeledStrategy (Context Expr)
findFactorsStrategy = cleanUpStrategyAfter (applyTop cleanUpSimple) $
   label "find factors" $ repeatS findFactorsStrategyG

findFactorsStrategyG :: IsTerm a => LabeledStrategy (Context a)
findFactorsStrategyG = label "find factor step" $
   somewhereTimes $
      use niceFactorsNew <|> use commonFactorVarNew
      <|> use factorVariablePower <|> use simplerLinearFactor

somewhereTimes :: IsStrategy f => f (Context a) -> Strategy (Context a)
somewhereTimes = traverse [ parentFilter p]
 where p c = if isTimesC c then [0 .. arity c-1] else []

isTimesC :: Context a -> Bool
isTimesC = maybe False (isJust . isTimes) . currentTerm

flipEquationS :: IsTerm a => Strategy (Context a)
flipEquationS = use (check conditionVarsRHS) <*> use flipEquation

-----------------------------------------------------------
-- Expanding factors of an expression

expandStrategy :: LabeledStrategy (Context Expr)
expandStrategy = cleanUpStrategyAfter (applyTop f . changeInContext g) $
   label "expand factors" $ repeatS (somewhere $
      use distributionSquare <|> use merge <|> use distributeTimes <|>
      use defPowerNat <|> use noDivisionConstant <|> use fractionProduct)
   <*>
      try (use ruleNormalizePolynomial)
 where -- mergeAlike
   f = transform (simplify (listOfPowerFactors "x" rationalView))
     -- . cleanUpSimple
   g = simplify (polyRelaxedForm rationalView)