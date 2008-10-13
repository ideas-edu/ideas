-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Exercises (dnfExercise) where

import Domain.Logic.Generator
import Domain.Logic.Formula
import Domain.Logic.Strategies
import Domain.Logic.Parser
import Domain.Logic.Rules
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Context
import Common.Parsing (fromRanged, subExpressionAt)
import Control.Monad
import Data.Maybe

-- Currently, we use the DWA strategy
dnfExercise :: Exercise Logic
dnfExercise = Exercise
   { identifier    = "Proposition to DNF" -- TODO: SIMPLIFY!
   , domain        = "logic"
   , description   = "Proposition to DNF" 
   , status        = Stable
   , parser        = \s -> case parseLogicPars s of
                              (p, []) -> Right (fromRanged p)
                              (_, xs) -> Left $ unlines xs
   , subTerm       = \s r -> case parseLogicPars s of
                                (p, []) -> fmap makeLocation (subExpressionAt r p)
                                _       -> Nothing
   , prettyPrinter = ppLogicPars
   , equivalence   = eqLogic
   , equality      = equalLogicAC
   , finalProperty = isDNF
   , ruleset       = map liftRuleToContext (logicRules ++ buggyRules)
   , strategy      = toDNF_DWA
   , generator     = generateLogic
   , suitableTerm  = \p -> let n = stepsRemaining (emptyPrefix toDNF_DWA) (inContext p)
                           in countEquivalences p < 2 && n >= 4 && n <= 12
   }