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
module Domain.RelationAlgebra.Exercises (cnfExercise) where

import Common.Library
import Data.Maybe
import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Generator
import Domain.RelationAlgebra.Parser
import Domain.RelationAlgebra.Rules
import Domain.RelationAlgebra.Strategies
import Prelude hiding (repeat)
import Test.QuickCheck

cnfExercise :: Exercise RelAlg
cnfExercise = makeExercise
   { exerciseId     = describe "To conjunctive normal form" $
                         newId "relationalgebra.cnf"
   , status         = Alpha
   , parser         = parseRelAlg
   , prettyPrinter  = ppRelAlg
   , equivalence    = withoutContext probablyEqual -- isEquivalent
   , extraRules     = map liftToContext (relAlgRules ++ buggyRelAlgRules)
   , strategy       = toCNF
   , navigation     = navigator
   , ready          = predicate (myReady cnfExercise)
   , randomExercise = let ok p = let n = fromMaybe maxBound (stepsRemaining 4 p)
                                 in n >= 2 && n <= 4
                      in useGenerator ok (\_ -> templateGenerator 1)
   , testGenerator  = Just arbitrary
   }

stepsRemaining :: Int -> RelAlg -> Maybe Int
stepsRemaining i =
   lengthMax i . derivationTree False toCNF . inContext cnfExercise

{- cnfExerciseSimple :: Exercise RelAlg
cnfExerciseSimple = cnfExercise
   { identifier  = "cnf-simple"
   , description = description cnfExercise ++ " (simple)"
   , strategy    = label "Apply rules exhaustively" $ repeat $ somewhere $ alternatives $ ruleset cnfExercise
   } -}

myReady :: Exercise a -> a -> Bool
myReady ex = null . applyAll (alternatives $ filter (not . isBuggy) (ruleset ex))
         . inContext ex