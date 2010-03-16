-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
   ( linearStrategy, quadraticStrategy
   , higherDegreeStrategy, findFactorsStrategy
   ) where

import Prelude hiding (repeat, replicate, fail)
import Common.Apply
import Common.Strategy
import Common.Navigator
import Common.Transformation
import Common.View
import Common.Context
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Polynomial.Rules
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Polynomial.CleanUp

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Equation Expr)
linearStrategy = cleanUpStrategy (fmap cleanUpSimple) $
   label "Linear Equation" 
    $  label "Phase 1" (repeat (
          removeDivision 
          <|> ruleMulti (ruleSomewhere distributeTimes)
          <|> ruleMulti merge))
   <*> label "Phase 2" (repeat (
          (flipEquation |> varToLeft)
          <|> coverUpPlus id
          <|> coverUpTimes
          <|> coverUpNegate))

-- helper strategy
coverUpPlus :: (Rule (Equation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]

------------------------------------------------------------
-- Quadratic equations

quadraticStrategy :: LabeledStrategy (Context (OrList (Relation Expr)))
quadraticStrategy = cleanUpStrategy (change cleanUpRelation) $ 
   label "Quadratic Equation Strategy" $ 
   repeat $  fromEquation generalForm
          |> mapRules (liftRule (contextView (switchView equationView))) generalABCForm
          |> fromEquation zeroForm 
          |> fromEquation constantForm
          |> simplifyForm
          |> fromEquation topForm 
 where
   fromEquation = mapRules (liftToContext . liftRule (switchView equationView))
 
   generalForm = label "general form" $ 
      ruleOnce commonFactorVar 
      <|> ruleOnce noLinFormula{- or coverup -}
      <|> ruleOnce simplerA <|> ruleOnce niceFactors 
      <|> coverUpPower -- to deal with special case x^2=0
      
   generalABCForm = label "abc form" abcFormula
 
   zeroForm = label "zero form" $
      toStrategy mulZero
         
   constantForm = label "constant form" $ 
      coverUpPower <|> ruleOnce coverUpTimes <|> coverUpPlus ruleOnce
      <|> ruleOnce coverUpNegate <|> ruleOnce coverUpNumerator 
      <|> squareBothSides <|> ruleOnce factorLeftAsSquare 
         
   simplifyForm = (fromEquation $ 
      label "square root simplification" $ 
           toStrategy (ruleMulti2 (ruleSomewhere simplerSquareRoot)))
        <|> remove (label "approximate result" $ 
            toStrategy $ liftToContext (ruleMulti ruleApproximate))

   topForm = label "top form" $
      ( ruleOnce2 (ruleSomewhere merge) 
        <|> ruleOnce cancelTerms  
        <|> ruleMulti2 (ruleSomewhere distributionSquare)
        <|> ruleMulti2 (ruleSomewhere distributeTimes) 
        <|> ruleMulti2 (ruleSomewhere distributeDivision)
        <|> ruleOnce flipEquation)
      |> (ruleOnce moveToLeft <|> remove (ruleOnce prepareSplitSquare))
   -- to do: find a better location in the strategy for splitting the square
   
-----------------------------------------------------------
-- Higher degree equations

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
             ] ++ [coverUpPlus ruleOnce]
   f = mapRulesS (liftToContext . liftRule (switchView equationView))
   
   specialV :: View (Context (OrList (Relation Expr))) (Context (OrList (Equation Expr)))
   specialV = contextView (switchView equationView)

{-
isQ2 :: Context (OrList (Relation Expr)) -> Bool
isQ2 = maybe False isQ . match (switchView equationView) . fromContext

isQ :: OrList (Equation Expr) -> Bool
isQ = (`belongsTo` quadraticEquationsView)
-}

-- like ruleOnce: TODO, replace!
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

-----------------------------------------------------------
-- Finding factors in an expression

findFactorsStrategy :: LabeledStrategy Expr
findFactorsStrategy = cleanUpStrategy cleanUpSimple $
   label "find factors" $
   repeat (niceFactorsNew <|> commonFactorVarNew)