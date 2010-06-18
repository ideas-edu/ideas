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
-- Exercise for the logic domain: to prove two propositions equivalent
--
-----------------------------------------------------------------------------
module Domain.Logic.Proofs where

import Common.Exercise
import Domain.Logic.Formula

-- Currently, we use the DWA strategy
proofExercise :: Exercise (SLogic, SLogic)
proofExercise = makeExercise
   { exerciseId     = describe "Prove two propositions equivalent" $
                         newId "logic.proof"
--   , status         = Stable
--   , parser         = parseLogicPars
--   , prettyPrinter  = ppLogicPars
--   , equivalence    = eqLogic
--   , similarity     = equalLogicA
--   , isReady        = isDNF
--   , isSuitable     = suitable
--   , extraRules     = map liftToContext (logicRules ++ buggyRules)
--   , strategy       = dnfStrategyDWA
--   , navigation     = navigator
--   , difference     = differenceMode eqLogic
--   , testGenerator  = Just (restrictGenerator suitable arbitrary)
--   , randomExercise = useGenerator (const True) logicExercise
   }