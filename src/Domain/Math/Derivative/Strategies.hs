{-# OPTIONS -fno-case-merge #-}
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
   ( derivativeStrategy, derivativePolyStrategy, getDiffExpr
   ) where

import Common.Library
import Data.Maybe
import Domain.Math.Derivative.Rules 
import Domain.Math.Expr
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.Rules
import Domain.Math.Numeric.Views

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
      repeatS (somewhere (alternatives list))
      <*> derivativePolyStepStrategy
 where
   list = map liftToContext
      [ distributionSquare, distributeTimes, merge
      , distributeDivision, noDivisionConstant
      ]

derivativePolyStepStrategy :: LabeledStrategy (Context Expr)
derivativePolyStepStrategy = label "derivative-poly-step" $
   check polyDiff <*> liftToContext ruleDerivPolynomial
 where
   polyDiff = maybe False nfPoly . (>>= getDiffExpr) . current
   nfPoly   = (`belongsTo` polyNormalForm rationalView)

isDiff :: Expr -> Bool
isDiff = isJust . getDiffExpr

getDiffExpr :: Expr -> Maybe Expr
getDiffExpr (Sym d [Sym l [Var _, expr]]) | 
   d == diffSymbol && l == lambdaSymbol = Just expr
getDiffExpr _ = Nothing