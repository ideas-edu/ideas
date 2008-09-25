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
-- (todo)
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
import Common.Strategy hiding (not)
import Common.Transformation
import Test.QuickCheck hiding (label)

cnfExercise :: Exercise RelAlg
cnfExercise = makeExercise
   { identifier    = "cnf"
   , domain        = "relationalg"
   , description   = "To conjunctive normal form"
   , status        = Experimental
   , parser        = \s -> case parseRelAlg s of
                              (p, [])   -> Right p
                              (_, msgs) -> Left  (show msgs)
   , prettyPrinter = ppRelAlg
   , equivalence = probablyEqual
   , ruleset   = map liftRuleToContext relAlgRules
   , strategy  = toCNF
   , finalProperty = ready (ruleset cnfExercise)
   , generator = oneof [gen1,gen2,gen3,gen4,gen5,gen6,gen7,gen8,gen9]
   , suitableTerm = not . isCNF
   }

{- cnfExerciseSimple :: Exercise RelAlg
cnfExerciseSimple = cnfExercise
   { identifier  = "cnf-simple"
   , description = description cnfExercise ++ " (simple)"
   , strategy    = label "Apply rules exhaustively" $ repeat $ somewhere $ alternatives $ ruleset cnfExercise
   } -}
   
ready :: [Rule (Context a)] -> a -> Bool
ready rs = null . applyAll (alternatives rs) . inContext