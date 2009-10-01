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
module Domain.RelationAlgebra.Exercises (cnfExercise) where

import Prelude hiding (repeat)
import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Generator
import Domain.RelationAlgebra.Strategies
import Domain.RelationAlgebra.Rules
import Domain.RelationAlgebra.Parser
import Common.Apply
import Common.Exercise
import Common.Context
import Text.Parsing (fromRanged)
import Common.Rewriting (treeDiff)
import Common.Strategy hiding (not)
import Common.Transformation

cnfExercise :: Exercise RelAlg
cnfExercise = testableExercise
   { description    = "To conjunctive normal form"
   , exerciseCode   = makeCode "relationalg" "cnf"
   , status         = Provisional
   , parser         = either Left (Right . fromRanged) . parseRelAlg
   , prettyPrinter  = ppRelAlg
   , equivalence    = probablyEqual -- isEquivalent
   , ruleset        = map liftToContext (relAlgRules ++ buggyRelAlgRules)
   , strategy       = toCNF
   , differences    = treeDiff
   , ordering       = compare
   , isReady        = ready (ruleset cnfExercise)
   , randomExercise = let ok p =
                             let n = stepsRemaining (emptyPrefix toCNF) (inContext p)
                             in n >= 2 && n <= 4
                      in useGenerator ok (templateGenerator 1)
   }

{- cnfExerciseSimple :: Exercise RelAlg
cnfExerciseSimple = cnfExercise
   { identifier  = "cnf-simple"
   , description = description cnfExercise ++ " (simple)"
   , strategy    = label "Apply rules exhaustively" $ repeat $ somewhere $ alternatives $ ruleset cnfExercise
   } -}
   
ready :: [Rule (Context a)] -> a -> Bool
ready rs = null . applyAll (alternatives $ filter (not . isBuggyRule) rs) . inContext