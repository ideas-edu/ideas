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
import Common.Derivation
import Common.Rewriting (differenceMode)
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Navigator

cnfExercise :: Exercise RelAlg
cnfExercise = testableExercise
   { exerciseId     = describe "To conjunctive normal form" $
                         newId "relationalg.cnf"
   , status         = Alpha
   , parser         = parseRelAlg
   , prettyPrinter  = ppRelAlg
   , equivalence    = probablyEqual -- isEquivalent
   , extraRules     = map liftToContext (relAlgRules ++ buggyRelAlgRules)
   , strategy       = toCNF
   , navigation     = navigator
   , difference     = differenceMode probablyEqual
   , ordering       = compare
   , isReady        = ready cnfExercise
   , randomExercise = let ok p = let n = fromMaybe maxBound (stepsRemaining 4 p)
                                 in n >= 2 && n <= 4
                      in useGenerator ok (\_ -> templateGenerator 1)
   }

stepsRemaining :: Int -> RelAlg -> Maybe Int
stepsRemaining i = 
   lengthMax i . derivationTree toCNF . inContext cnfExercise

{- cnfExerciseSimple :: Exercise RelAlg
cnfExerciseSimple = cnfExercise
   { identifier  = "cnf-simple"
   , description = description cnfExercise ++ " (simple)"
   , strategy    = label "Apply rules exhaustively" $ repeat $ somewhere $ alternatives $ ruleset cnfExercise
   } -}
   
ready :: Exercise a -> a -> Bool
ready ex = null . applyAll (alternatives $ filter (not . isBuggyRule) (ruleset ex)) 
         . inContext ex