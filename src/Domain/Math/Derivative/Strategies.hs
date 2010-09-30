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
module Domain.Math.Derivative.Strategies (derivativeStrategy) where

import Common.Context
import Common.Navigator
import Common.Strategy
import Domain.Math.Derivative.Rules 
import Domain.Math.Expr
import Domain.Math.Polynomial.CleanUp
import Prelude hiding (repeat)

derivativeStrategy :: LabeledStrategy (Context Expr)
derivativeStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "Derivative" $ repeat $ somewhere $ 
      alternatives (map liftToContext derivativeRules)
      <|> check isDiffC <*> once (once (liftToContext ruleDefRoot))
 where
   isDiffC = maybe False isDiff . current
   
isDiff :: Expr -> Bool
isDiff (Sym d [Sym l [Var _, _]]) = 
   d == diffSymbol && l == lambdaSymbol
isDiff _ = False