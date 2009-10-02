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
-- Exercise for the logic domain, used for the OUNL course 
-- "Discrete Wiskunde A (DWA)"
--
-----------------------------------------------------------------------------
module Domain.Logic.Exercises 
   ( dnfExercise, dnfUnicodeExercise
   ) where

import Domain.Logic.Generator
import Domain.Logic.Formula
import Domain.Logic.Strategies
import Domain.Logic.Parser
import Domain.Logic.Rules
import Common.Exercise
import Common.Strategy hiding (not, label)
import Common.Transformation (isMajorRule)
import Common.Context
import Text.Parsing (fromRanged)
import Common.Rewriting
import Test.QuickCheck

-- Currently, we use the DWA strategy
dnfExercise :: Exercise SLogic
dnfExercise = makeExercise
   { description    = "Proposition to DNF"
   , exerciseCode   = makeCode "logic" "dnf"
   , status         = Stable
   , parser         = either Left (Right . fromRanged) . parseLogicPars
   , prettyPrinter  = ppLogicPars
   , equivalence    = eqLogic
   , similarity     = equalLogicA
   , isReady        = isDNF
   , isSuitable     = suitable
   , extraRules     = map liftToContext (logicRules ++ buggyRules)
   , strategy       = dnfStrategyDWA
   , differences    = treeDiff
   , testGenerator  = Just (restrictGenerator suitable arbitrary)
   , randomExercise = let ok p = let n = steps p
                                 in countEquivalences p <= 2 && n >= 4 && n <= 12
                         in useGenerator ok generateLogic
   }

-- Direct support for unicode characters
dnfUnicodeExercise :: Exercise SLogic
dnfUnicodeExercise = dnfExercise
   { description   = description dnfExercise ++ " (unicode support)"
   , exerciseCode  = makeCode "logic" "dnf-unicode"
   , parser        = either Left (Right . fromRanged) . parseLogicUnicodePars
   , prettyPrinter = ppLogicUnicodePars
   }

suitable :: SLogic -> Bool
suitable = (<=2) . countEquivalences

steps :: SLogic -> Int
steps p =
   case derivations (unlabel dnfStrategyDWA) (inContext p) of
      (_, xs):_ -> length (take 15 (filter (isMajorRule . fst) xs))
      _         -> 15

-- QuickCheck property to monitor the number of steps needed 
-- to normalize a random proposition (30-40% is ok)
testGen :: Property
testGen = forAll generateLogic $ \p -> 
   let n = steps p
   in countEquivalences p <= 2 ==> label (show (n >= 4 && n <= 12)) True
   
{- main :: IO ()
main = quickCheck testGen -}