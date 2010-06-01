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
   ( linearStrategy, linearMixedStrategy, quadraticStrategy, quadraticStrategyNEW
   , higherDegreeStrategy, findFactorsStrategy, exprNavigator, higherDegreeStrategyNEW
   ) where

import Prelude hiding (repeat, replicate, fail)
import Common.Apply
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

use :: IsTerm a => Rule a -> Rule (Context b)
use = liftRuleIn (makeView f g)
 where
   f c = currentT c >>= fromExpr >>= \b -> Just (b, c)
   g (e, c) = fromJust (replaceT (toExpr e) c)

useEq :: IsTerm a => Rule (Equation a) -> Rule (Context b)
useEq = use

useC :: (IsTerm a, IsTerm b) => Rule (Context a) -> Rule (Context b)
useC = liftRule (makeView (castT exprView) (fromJust . castT exprView))
   
cleanExpr :: (Expr -> Expr) -> Context a -> Context a
cleanExpr f c = case top c >>= changeT (return . f) of
                  Just ok -> navigateTowards (location c) ok
                  Nothing -> c
                  
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
linearStrategy = cleanUpStrategy (cleanExpr cleanUpSimple) linearStrategyG

linearMixedStrategy :: LabeledStrategy (Context (Equation Expr))
linearMixedStrategy = 
   let f   = cleanExpr (transform (simplify mixedFractionView) . cleanUpSimple)
       cfg = [ (ByName (name ruleNormalizeMixedFraction), Reinsert)
             , (ByName (name ruleNormalizeRational), Remove)
             ] 
   in configure cfg (cleanUpStrategy f linearStrategyG)

linearStrategyG :: IsTerm a => LabeledStrategy (Context a)
linearStrategyG =
   label "Linear Equation" $
       label "Phase 1" (repeat (
               useEq removeDivision
          <|>  multi (name distributeTimes) (somewhere (useC parentNotNegCheck <*> use distributeTimes))
          <|>  multi "merge similar terms" (once (use merge))))
   <*> label "Phase 2" (repeat (
              (use flipEquation |> useEq varToLeft)
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

quadraticStrategyNEW :: LabeledStrategy (Context (OrList (Relation Expr)))
quadraticStrategyNEW = cleanUpStrategy (cleanTop cleanUpRelation) quadraticStrategyNEWG

quadraticStrategyNEWG :: IsTerm a => LabeledStrategy (Context a)
quadraticStrategyNEWG = 
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

higherDegreeStrategyNEW :: LabeledStrategy (Context (OrList (Relation Expr)))
higherDegreeStrategyNEW = cleanUpStrategy (cleanTop cleanUpRelation) higherDegreeStrategyNEWG

higherDegreeStrategyNEWG :: IsTerm a => LabeledStrategy (Context a)
higherDegreeStrategyNEWG = label "higher degree" $ 
   higherForm 
   <*> label "quadratic"  quadraticStrategyNEWG
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
      useC substBackVarT  <*> repeat (somewhere (use coverUpPower)) 

substBackVarT :: Rule (Context (OrList (Relation Expr)))
substBackVarT = substBackVar

-----------------------------------------------------------
-- Finding factors in an expression

findFactorsStrategy :: LabeledStrategy Expr
findFactorsStrategy = cleanUpStrategy cleanUpSimple $
   label "find factors" $
   repeat (niceFactorsNew <|> commonFactorVarNew)
   
   
quadraticStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
quadraticStrategy = cleanUpStrategy (change cleanUpRelation) $ 
   label "Quadratic Equation Strategy" $ 
   repeat $  -- Relaxed strategy: even if there are "nice" factors, allow use of square formula
          (  fromEquation generalForm
         <|> mapRules (liftRule (contextView (switchView equationView))) generalABCForm
          )
          |> fromEquation zeroForm 
          |> fromEquation constantForm
          |> simplifyForm
          |> fromEquation topForm 
 where
   fromEquation = mapRules (liftToContext . liftRule (switchView equationView))
 
   -- ax^2 + bx + c == 0, without square formula
   generalForm = label "general form" $ 
      ruleOnce commonFactorVar 
      <|> ruleOnce noLinFormula{- or coverup -}
      <|> ruleOnce simplerPolynomial <|> remove (ruleOnce bringAToOne)
      <|> ruleOnce niceFactors 
      <|> coverUpPower -- to deal with special case x^2=0
      
   generalABCForm = label "abc form" abcFormula
 
   zeroForm = label "zero form" $
      toStrategy mulZero
    
   -- expr == c
   constantForm = label "constant form" $ 
      -- coverUpPower <|> -- never used, see coverUpPower rule in general form
      ruleOnce coverUpTimes <|> coverUpPlus ruleOnce
      <|> ruleOnce coverUpNegate <|> ruleOnce coverUpNumerator 
      <|> squareBothSides <|> ruleOnce factorLeftAsSquare 

   -- simplifies square roots, or do an approximation 
   simplifyForm = (fromEquation $ 
      label "square root simplification" $ 
           toStrategy (ruleMulti2 (ruleSomewhere simplerSquareRoot)))
        <|> remove (label "approximate result" $ 
            toStrategy $ liftToContext (ruleMulti ruleApproximate))

   topForm = label "top form" $
        (ruleOnce cancelTerms  <|> sameFactor)
      |> (  ruleOnce sameConFactor
        <|> ruleOnce2  (ruleSomewhere merge) 
        <|> ruleMulti2 (ruleSomewhere distributionSquare)
        <|> ruleMulti2 distributeTimesSomewhere 
        <|> ruleMulti2 (ruleSomewhere distributeDivision)
        <|> ruleOnce flipEquation
         )
      |> (ruleOnce moveToLeft <|> remove (ruleOnce prepareSplitSquare))
   -- to do: find a better location in the strategy for splitting the square
   
-- helper strategy
coverUpPlus :: (Rule (Equation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]
   
higherDegreeStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
higherDegreeStrategy =
   label "higher degree" $ 
      higherForm <*> label "quadratic" ({-option (check isQ2 <*> -} quadraticStrategy)
      <*> 
      cleanUpStrategy (change cleanUpRelation) (label "afterwards" (try (substBackVar <*> f (repeat coverUpPower))))
 where
   higherForm = cleanUpStrategy (change cleanUpRelation) $ 
      label "higher degree form" $
      repeat (f (toStrategy allPowerFactors) |> 
         (f (alternatives list) <|> liftRule specialV (ruleOrCtxOnce higherSubst))
            |> f (toStrategy (ruleOnce moveToLeft)))
   list = map toStrategy  
             [ coverUpPower, ruleOnce coverUpTimes
             , mulZero, {-ruleOnce2 powerFactor,-} sameFactor
             , ruleOnce exposeSameFactor
             ] ++ [coverUpPlus ruleOnce] ++ [toStrategy (ruleOnce sameConFactor)]
   f = mapRulesS (liftToContext . liftRule (switchView equationView))
   
   specialV :: View (Context (OrList (Relation Expr))) (Context (OrList (Equation Expr)))
   specialV = contextView (switchView equationView)

{-# DEPRECATED ruleOrCtxOnce "Replace ruleOrCtxOnce" #-}
ruleOrCtxOnce :: Rule (Context a) -> Rule (Context (OrList a))
ruleOrCtxOnce r = makeSimpleRuleList (name r) $ \ctx -> do
   let env = getEnvironment ctx
   a <- fromContext ctx
   case disjunctions a of
      Just xs -> f [] env xs
      Nothing -> []
 where
   f _   _   [] = []
   f acc env (a:as) = 
      case applyAll r (newContext env (noNavigator a)) of
         []  -> f (a:acc) env as
         new -> concatMap (fmapC $ \na -> orList (reverse acc++na:as)) new
   fmapC g c = 
      case fromContext c of
         Just a  -> [newContext (getEnvironment c) (noNavigator (g a))]
         Nothing -> []