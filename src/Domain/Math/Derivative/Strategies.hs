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
module Domain.Math.Derivative.Strategies 
   ( derivativeStrategy, derivativePolyStrategy
   , derivativeProductStrategy, derivativeQuotientStrategy
   , derivativePowerStrategy, getDiffExpr
   ) where

import Common.Library
import Data.Maybe
import Domain.Math.Derivative.Rules 
import Domain.Math.Expr
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.Rules
import Domain.Math.Numeric.Views
import Domain.Math.Power.Strategies
import Domain.Math.Power.Rules

import Prelude hiding ((^))

derivativeStrategy :: LabeledStrategy (Context Expr)
derivativeStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "Derivative" $ repeatS $ somewhere $ 
      alternatives (map liftToContext derivativeRules)
      <|> derivativePolyStepStrategy
      <|> check isDiffC <*> once (once (liftToContext ruleDefRoot))
 where
   isDiffC = maybe False isDiff . current

derivativePolyStrategy :: LabeledStrategy (Context Expr)
derivativePolyStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "derivative-polynomial" $
      repeatS (somewhere (alternatives (map liftToContext rulesPolyNF)))
      <*> derivativePolyStepStrategy

rulesPolyNF :: [Rule Expr]
rulesPolyNF =
   [ distributionSquare, distributeTimes, merge
   , distributeDivision, noDivisionConstant
   ]

derivativeProductStrategy :: LabeledStrategy (Context Expr)
derivativeProductStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "derivative-product" $
      repeatS (somewhere (derivativePolyStepStrategy |> alternatives list))
 where
   list = map liftToContext
      [ distributeDivision, noDivisionConstant
      , ruleDerivProduct, defPowerNat
      , ruleDerivNegate, ruleDerivPlus, ruleDerivMin
      ]

derivativeQuotientStrategy :: LabeledStrategy (Context Expr)
derivativeQuotientStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "derivative-quotient" $
   repeatS (somewhere (derivativePolyStepStrategy |> alternatives list))
   <*> repeatS (exceptLowerDiv (alternatives (map liftToContext rulesPolyNF)))
 where
   list = map liftToContext
      [ ruleDerivQuotient, ruleDerivPlus, ruleDerivMin, ruleDerivNegate ]
      
derivativePowerStrategy :: LabeledStrategy (Context Expr)
derivativePowerStrategy = label "derivative-power" $ 
   cleanUpStrategy (applyTop cleanUpExpr2) (label "split-rational" 
      (repeatS (somewhere (liftToContext ruleSplitRational)))) <*>
   configure mycfg powerOfStrategy <*>
   cleanUpStrategy (applyTop cleanUpExpr2) (label "use-derivative-rules" 
      (repeatS (somewhere (alternatives list))))
 where
   list = map liftToContext
      [ ruleDerivPlus, ruleDerivMin, ruleDerivNegate, ruleDerivPowerFactor ]
   mycfg = [(ByName (showId myFractionTimes), Remove)]
      
derivativePolyStepStrategy :: LabeledStrategy (Context Expr)
derivativePolyStepStrategy = label "derivative-poly-step" $
   check polyDiff <*> liftToContext ruleDerivPolynomial
 where
   polyDiff = maybe False nfPoly . (>>= getDiffExpr) . current
   nfPoly   = (`belongsTo` polyNormalForm rationalView)

exceptLowerDiv :: IsStrategy f => f (Context Expr) -> Strategy (Context Expr)
exceptLowerDiv = somewhereWith "except-lower-div" $ \a -> 
   if (isDivC a) then [0] else [0 .. arity a-1]
 where 
   isDivC = maybe False isDiv . current
   isDiv (_ :/: _) = True
   isDiv _         = False