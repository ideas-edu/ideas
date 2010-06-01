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
   ( linearStrategy, linearMixedStrategy, findFactorsStrategy
   , quadraticStrategy, quadraticStrategyG
   , higherDegreeStrategy, higherDegreeStrategyG
   , exprNavigator
   , cleanTop, useC, use, multi, parentNotNegCheck
   ) where

import Prelude hiding (repeat, replicate, fail)
import Common.Strategy
import Common.Navigator
import Common.Transformation
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
import Common.Rewriting (IsTerm)
import Data.Maybe

use :: (IsTerm a, IsTerm b) => Rule a -> Rule (Context b)
use = useC . liftToContext

useC :: (IsTerm a, IsTerm b) => Rule (Context a) -> Rule (Context b)
useC = liftRule (makeView (castT exprView) (fromJust . castT exprView))
               
cleanTop :: (a -> a) -> Context a -> Context a
cleanTop f c = 
   case top c of 
      Just ok -> navigateTowards (location c) (change f ok)
      Nothing -> c
                  
exprNavigator :: IsTerm a => a -> Navigator a
exprNavigator a = 
   let f = castT exprView . viewNavigator . toExpr
   in fromMaybe (noNavigator a) (f a)

multi :: IsStrategy f => String -> f a -> LabeledStrategy a
multi s = collapse . label s . repeat1

parentNotNegCheck :: Rule (Context Expr)
parentNotNegCheck = minorRule $ makeSimpleRule "parent not negate check" $ \c -> 
   case up c >>= current of
      Just (Negate _) -> Nothing
      _               -> Just c

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Context (Equation Expr))
linearStrategy = cleanUpStrategy (cleanTop (fmap cleanUpSimple)) linearStrategyG

linearMixedStrategy :: LabeledStrategy (Context (Equation Expr))
linearMixedStrategy = 
   let f   = cleanTop (fmap (transform (simplify mixedFractionView) . cleanUpSimple))
       cfg = [ (ByName (name ruleNormalizeMixedFraction), Reinsert)
             , (ByName (name ruleNormalizeRational), Remove)
             ] 
   in configure cfg (cleanUpStrategy f linearStrategyG)

linearStrategyG :: IsTerm a => LabeledStrategy (Context a)
linearStrategyG =
   label "Linear Equation" $
       label "Phase 1" (repeat (
               use removeDivision
          <|>  multi (name distributeTimes) (somewhere (useC parentNotNegCheck <*> use distributeTimes))
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
   cleanUpStrategy (cleanTop cleanUpRelation) quadraticStrategyG

quadraticStrategyG :: IsTerm a => LabeledStrategy (Context a)
quadraticStrategyG = 
   label "Quadratic Equation Strategy" $ repeat $
   -- Relaxed strategy: even if there are "nice" factors, allow use of quadratic formula
      somewhere (
         (generalForm <|> generalABCForm)
         |> zeroForm 
         |> constantForm)
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
         multi (name simplerSquareRoot) (somewhere (use simplerSquareRoot)))
      <|> 
      remove (label "approximate result" (
         multi (name ruleApproximate) (somewhere (use ruleApproximate))))

   topForm = label "top form" $
        somewhere (use cancelTerms  <|> use sameFactor)
      |> (  somewhere (use sameConFactor)
        <|> multi (name merge) (somewhere (use merge))
        <|> somewhere (use distributionSquare)
        <|> multi (name distributeTimes) (somewhere 
               (useC parentNotNegCheck <*> use distributeTimes))
        <|> multi (name distributeDivision) (somewhere 
               (once (use distributeDivision)))
        <|> somewhere (use flipEquation)
         )
      |> somewhere (use moveToLeft <|> remove (use prepareSplitSquare))

-----------------------------------------------------------
-- Higher degree equations

higherDegreeStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
higherDegreeStrategy = 
   cleanUpStrategy (cleanTop cleanUpRelation) higherDegreeStrategyG

higherDegreeStrategyG :: IsTerm a => LabeledStrategy (Context a)
higherDegreeStrategyG = label "higher degree" $ 
   higherForm 
   <*> label "quadratic"  quadraticStrategyG
   <*> afterSubst
 where
   higherForm = label "higher degree form" $ repeat $
      use allPowerFactors 
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

findFactorsStrategy :: LabeledStrategy Expr
findFactorsStrategy = cleanUpStrategy cleanUpSimple $
   label "find factors" $
   repeat (niceFactorsNew <|> commonFactorVarNew)