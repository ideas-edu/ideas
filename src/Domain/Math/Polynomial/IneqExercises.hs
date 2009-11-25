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
module Domain.Math.Polynomial.IneqExercises 
   ( ineqLinearExercise, ineqQuadraticExercise, ineqHigherDegreeExercise
   ) where

import Prelude hiding (repeat)
import Control.Monad
import Common.Context
import Common.Exercise
import Common.Strategy
import Common.Transformation
import Common.View
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Examples.DWO2
import Domain.Math.Polynomial.Rules 
import Domain.Math.Polynomial.CleanUp

ineqLinearExercise :: Exercise (Relation Expr)
ineqLinearExercise = makeExercise 
   { description  = "solve a linear inequation"
   , exerciseCode = makeCode "math" "linineq"
   , strategy     = mapRules ignoreContext ineqLinear
   , examples     = map (build inequalityView) (concat ineqLin1 ++ [extra])
   }
   
ineqQuadraticExercise :: Exercise (Relation Expr)
ineqQuadraticExercise = makeExercise 
   { description  = "solve a quadratic inequation"
   , exerciseCode = makeCode "math" "quadrineq"
   , examples     = map (build inequalityView) (concat $ ineqQuad1 ++ [ineqQuad2])
   }

ineqHigherDegreeExercise :: Exercise (Relation Expr)
ineqHigherDegreeExercise = makeExercise 
   { description  = "solve an inequation of higher degree"
   , exerciseCode = makeCode "math" "ineqhigherdegree"
   , examples     = map (build inequalityView) ineqHigh
   }

ineqLinear :: LabeledStrategy (Relation Expr)
ineqLinear = cleanUpStrategy (fmap cleanUpSimple) $
   label "Linear inequation" $
      label "Phase 1" (repeat (
             removeDivision
         <|> ruleMulti (ruleSomewhere distributeTimes)
         <|> ruleMulti merge))
      <*>  
      label "Phase 2" (
         try varToLeft 
         <*> try (coverUpPlus id)
         <*> try flipSign
         <*> try coverUpTimesPositive)

-- helper strategy
coverUpPlus :: (Rule (Relation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [ coverUpBinaryRule "plus" (commOp . isPlus) (-) 
   , coverUpBinaryRule "minus left" isMinus (+)
   , coverUpBinaryRule "minus right" (flipOp . isMinus) (flip (-))
   ] -- [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]
   
coverUpTimesPositive :: Rule (Relation Expr)
coverUpTimesPositive = coverUpBinaryRule "times positive" (commOp . m) (/) varConfig
 where
   m expr = do
      (a, b) <- matchM timesView expr
      r <- matchM rationalView a
      guard (r>0)
      return (a, b)
      
flipSign :: Rule (Relation Expr)
flipSign = makeSimpleRule "flip sign" $ \r -> do
   let lhs = leftHandSide r
       rhs = rightHandSide r
   guard (isNegative lhs) 
   return $ constructor (flipSides r) (neg lhs) (neg rhs)
 where
   isNegative (Negate _) = True
   isNegative expr = 
      maybe False fst (match productView expr)
      
extra = (x-12) / (-2) :>: (x+3)/3
 where x = Var "x"