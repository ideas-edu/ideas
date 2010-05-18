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
   ( linearStrategy, linearMixedStrategy, quadraticStrategy
   , higherDegreeStrategy, findFactorsStrategy
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

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Equation Expr)
linearStrategy = linearStrategyWith False

linearMixedStrategy :: LabeledStrategy (Equation Expr)
linearMixedStrategy = linearStrategyWith True

linearStrategyWith :: Bool -> LabeledStrategy (Equation Expr)
linearStrategyWith mixed = cleanUpStrategy (fmap clean) $
   label "Linear Equation" 
    $  label "Phase 1" (repeat (
              removeDivision 
          <|> ruleMulti distributeTimesSomewhere
          <|> ruleMulti merge))
   <*> label "Phase 2" (repeat (
          (flipEquation |> varToLeft)
          <|> coverups))
   <*> try (ruleMulti final)
 where
   coverups = coverUpPlus id <|> coverUpTimes <|> coverUpNegate
   (clean, final) 
      | mixed = 
           ( transform (simplify mixedFractionView) . cleanUpSimple
           , ruleNormalizeMixedFraction
           )
      | otherwise = 
          (cleanUpSimple, ruleNormalizeRational)
      
-- helper strategy
coverUpPlus :: (Rule (Equation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]

------------------------------------------------------------
-- Quadratic equations

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
             ] ++ [coverUpPlus ruleOnce] ++ [toStrategy (ruleOnce sameConFactor)]
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