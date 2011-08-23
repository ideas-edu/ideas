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
module Domain.Math.Derivative.Strategies
   ( derivativeStrategy, derivativePolyStrategy
   , derivativeProductStrategy, derivativeQuotientStrategy
   , derivativePowerStrategy, getDiffExpr
   ) where

import Common.Library
import Data.Maybe
import Domain.Math.CleanUp
import Domain.Math.Derivative.Rules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Strategies

import Prelude hiding ((^))

derivativeStrategy :: LabeledStrategy (Context Expr)
derivativeStrategy = cleanUpStrategyAfter (applyTop cleanUpExpr) $
   label "Derivative" $ repeatS $ somewhere $
      alternatives (map liftToContext derivativeRules)
      <|> derivativePolyStepStrategy
      <|> check isDiffC <*> once (once (liftToContext ruleDefRoot))
 where
   isDiffC = maybe False isDiff . current

derivativePolyStrategy :: LabeledStrategy (Context Expr)
derivativePolyStrategy = cleanUpStrategyAfter (applyTop cleanUpExpr) $
   label "derivative-polynomial" $
      repeatS (somewhere (alternatives (map liftToContext rulesPolyNF)))
      <*> derivativePolyStepStrategy

rulesPolyNF :: [Rule Expr]
rulesPolyNF =
   [ distributionSquare, distributeTimes, merge
   , distributeDivision, noDivisionConstant
   ]

derivativeProductStrategy :: LabeledStrategy (Context Expr)
derivativeProductStrategy = cleanUpStrategyAfter (applyTop cleanUpExpr) $
   label "derivative-product" $
      repeatS (somewhere (derivativePolyStepStrategy |> alternatives list))
 where
   list = map liftToContext
      [ distributeDivision, noDivisionConstant
      , ruleDerivProduct, defPowerNat
      , ruleDerivNegate, ruleDerivPlus, ruleDerivMin
      ]

derivativeQuotientStrategy :: LabeledStrategy (Context Expr)
derivativeQuotientStrategy = cleanUpStrategyAfter (applyTop cleanUpExpr) $
   label "derivative-quotient" $
   repeatS (somewhere (derivativePolyStepStrategy |> alternatives list))
   <*> repeatS (exceptLowerDiv (alternatives (map liftToContext rulesPolyNF)))
 where
   list = map liftToContext
      [ ruleDerivQuotient, ruleDerivPlus, ruleDerivMin, ruleDerivNegate ]

derivativePowerStrategy :: LabeledStrategy (Context Expr)
derivativePowerStrategy = label "derivative-power" $
   cleanUpStrategyAfter (applyTop cleanUpExpr) (label "split-rational"
      (repeatS (somewhere (liftToContext ruleSplitRational)))) <*>
   configure mycfg simplifyPowerStrategy <*>
   repeatS (distr <*> configure mycfg simplifyPowerStrategy) <*>
   cleanUpStrategyAfter (applyTop cleanUpExpr) (label "use-derivative-rules"
      (repeatS (somewhere (alternatives list)))) <*>
   configure mycfg nonNegBrokenExpStrategy
 where
   list = map liftToContext
      [ ruleDerivPlus, ruleDerivMin, ruleDerivNegate, ruleDerivPowerFactor
      , ruleDerivCon ]
   mycfg = [(byName myFractionTimes, Remove)]
   distr = cleanUpStrategyAfter (applyTop cleanUpExpr) $
      label "distr" (somewhere (alternatives (map liftToContext rulesPolyNF)))

derivativePolyStepStrategy :: LabeledStrategy (Context Expr)
derivativePolyStepStrategy = label "derivative-poly-step" $
   check polyDiff <*> liftToContext ruleDerivPolynomial
 where
   polyDiff = maybe False nfPoly . (>>= getDiffExpr) . current
   nfPoly   = (`belongsTo` polyNormalForm rationalView)

exceptLowerDiv :: IsStrategy f => f (Context Expr) -> Strategy (Context Expr)
exceptLowerDiv = somewhereWith "except-lower-div" $ \a ->
   if isDivC a then [0] else [0 .. arity a-1]
 where
   isDivC = maybe False isDiv . current
   isDiv (_ :/: _) = True
   isDiv _         = False