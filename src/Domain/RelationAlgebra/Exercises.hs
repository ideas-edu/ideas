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
import Data.Maybe
import Text.Parsing (fromRanged)
import Common.Derivation
import Common.Rewriting (differenceMode)
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
   , extraRules     = map liftToContext (relAlgRules ++ buggyRelAlgRules)
   , strategy       = toCNF
   , difference     = differenceMode probablyEqual
   , ordering       = compare
   , isReady        = ready (ruleset cnfExercise)
   , randomExercise = let ok p = let n = fromMaybe maxBound (stepsRemaining 4 p)
                                 in n >= 2 && n <= 4
                      in useGenerator ok (\_ -> templateGenerator 1)
   }

stepsRemaining :: Int -> RelAlg -> Maybe Int
stepsRemaining i = 
   lengthMax i . derivationTree toCNF . inContext

{- cnfExerciseSimple :: Exercise RelAlg
cnfExerciseSimple = cnfExercise
   { identifier  = "cnf-simple"
   , description = description cnfExercise ++ " (simple)"
   , strategy    = label "Apply rules exhaustively" $ repeat $ somewhere $ alternatives $ ruleset cnfExercise
   } -}
   
ready :: [Rule (Context a)] -> a -> Bool
ready rs = null . applyAll (alternatives $ filter (not . isBuggyRule) rs) . inContext