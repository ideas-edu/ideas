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
import Common.Strategy hiding (not, label)
import Common.Context
import Common.Parsing (fromRanged, subExpressionAt)
import Common.Rewriting
import Control.Monad
import Data.Maybe
import Test.QuickCheck

-- Currently, we use the DWA strategy
dnfExercise :: Exercise Logic
dnfExercise = Exercise
   { identifier    = "Proposition to DNF" -- TODO: SIMPLIFY!
   , domain        = "logic"
   , description   = "Proposition to DNF" 
   , status        = Stable
   , parser        = either Left (Right . fromRanged) . parseLogicPars
   , subTerm       = \s r -> case parseLogicPars s of
                                Right p -> fmap makeLocation (subExpressionAt r p)
                                _       -> Nothing
   , prettyPrinter = ppLogicPars
   , equivalence   = eqLogic
   , equality      = equalLogicAC
   , finalProperty = isDNF
   , ruleset       = map liftRuleToContext (logicRules ++ buggyRules)
   , strategy      = toDNF_DWA
   , differences   = treeDiff
   , ordering      = compare
   , generator     = generateLogic
   , suitableTerm  = \p -> let n = stepsRemaining (emptyPrefix toDNF_DWA) (inContext p)
                           in countEquivalences p <= 2 && n >= 4 && n <= 12
   }
   
-- QuickCheck property to monitor the number of steps needed 
-- to normalize a random proposition (30-40% is ok)
testGen :: Property
testGen = forAll generateLogic $ \p -> 
   let n = stepsRemaining (emptyPrefix toDNF_DWA) (inContext p)
   in countEquivalences p <= 2 ==> label (show (n >= 4 && n <= 12)) True