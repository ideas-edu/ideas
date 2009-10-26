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
   , higherDegreeStrategy 
   ) where

import Prelude hiding (repeat, replicate, fail)
import Common.Strategy
import Common.Transformation
import Common.View
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Views
import Domain.Math.Data.OrList
import Domain.Math.Data.Equation
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
   <*> label "Phase 2" (
          try varToLeft 
          <*> try (coverUpPlus id) 
          <*> try (coverUpTimes |> try coverUpNegate))

-- helper strategy
coverUpPlus :: (Rule (Equation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]

------------------------------------------------------------
-- Quadratic equations

quadraticStrategy :: Bool -> LabeledStrategy (OrList (Equation Expr))
quadraticStrategy canUseABC = cleanUpStrategy cleanUp $ 
   label "Quadratic Equation Strategy" $ 
   repeat $ 
         -- general form
      (  label "general form" $ 
         ( ruleOnce commonFactorVar <|> ruleOnce noLinFormula{- or coverup -}
           <|> ruleOnce niceFactors <|> ruleOnce simplerA 
           <|> coverUpPower) -- to deal with special case x^2=0
         |> (if canUseABC then toStrategy abcFormula else fail)
      )
      |> -- zero form
      (  label "zero form"
         mulZero
      )
      |> -- constant form
      (  label "constant form" $ 
         coverUpPower <|> ruleOnce coverUpTimes <|> coverUpPlus ruleOnce
         <|> ruleOnce coverUpNegate <|> ruleOnce coverUpNumerator 
         <|> squareBothSides <|> ruleOnce factorLeftAsSquare 
      )
      |> -- simplification 
      (  label "square root simplification" $ 
         ruleMulti2 (ruleSomewhere simplerSquareRoot)
      )
      |> -- top form
      (  label "top form" $ 
         ( ruleOnce2 (ruleSomewhere merge) 
           <|> ruleOnce cancelTerms  
           <|> ruleMulti2 (ruleSomewhere distributionSquare)
           <|> ruleMulti2 (ruleSomewhere distributeTimes) 
           <|> ruleMulti2 (ruleSomewhere distributeDivision)
           <|> ruleOnce flipEquation)
         |> (ruleOnce moveToLeft <|> ruleOnce prepareSplitSquare)
      ) -- to do: find a better location in the strategy for splitting the square

-----------------------------------------------------------
-- Higher degree equations

higherDegreeStrategy :: LabeledStrategy (OrList (Equation Expr))
higherDegreeStrategy = cleanUpStrategy cleanUp $
   label "higher degree" $ 
      repeat (allPowerFactors |> (mulZero <|> ruleOnce2 powerFactor <|> sameFactor))
      <*> check isQ <*> quadraticStrategy True
 
isQ :: OrList (Equation Expr) -> Bool
isQ = (`belongsTo` quadraticEquationsView)