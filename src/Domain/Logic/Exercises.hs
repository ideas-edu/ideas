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
-- Exercise for the logic domain, used for the OUNL course 
-- "Discrete Wiskunde A (DWA)"
--
-----------------------------------------------------------------------------
module Domain.Logic.Exercises 
   ( dnfExercise, dnfUnicodeExercise
   ) where

import Common.Library
import Data.Maybe
import Domain.Logic.BuggyRules
import Domain.Logic.Formula
import Domain.Logic.Generator
import Domain.Logic.Parser
import Domain.Logic.Rules
import Domain.Logic.Strategies
import Test.QuickCheck

-- Currently, we use the DWA strategy
dnfExercise :: Exercise SLogic
dnfExercise = makeExercise
   { exerciseId     = describe "Proposition to DNF" $
                         newId "logic.propositional.dnf"
   , status         = Stable
   , parser         = parseLogicPars
   , prettyPrinter  = ppLogicPars
   , equivalence    = withoutContext eqLogic
   , similarity     = withoutContext equalLogicA
   , ready          = predicate isDNF
   , suitable       = predicate mySuitable
   , extraRules     = map liftToContext (extraLogicRules ++ buggyRules)
   , strategy       = dnfStrategyDWA
   , navigation     = navigator
   , testGenerator  = Just (restrictGenerator mySuitable arbitrary)
   , randomExercise = useGenerator (const True) logicExercise
   }

-- Direct support for unicode characters
dnfUnicodeExercise :: Exercise SLogic
dnfUnicodeExercise = dnfExercise
   { exerciseId    = describe "Proposition to DNF (unicode support)" $
                        newId "logic.propositional.dnf.unicode"
   , parser        = parseLogicUnicodePars
   , prettyPrinter = ppLogicUnicodePars
   }

logicExercise :: Difficulty -> Gen SLogic
logicExercise dif = 
   let (gen, (minStep, maxStep)) = generateLevel dif
       ok p = let i = fromMaybe maxBound (stepsRemaining maxStep p)
              in countEquivalences p <= 2 && i >= minStep && i <= maxStep
   in restrictGenerator ok gen

mySuitable :: SLogic -> Bool
mySuitable = (<=2) . countEquivalences

stepsRemaining :: Int -> SLogic -> Maybe Int
stepsRemaining i = 
   lengthMax i . derivationTree dnfStrategyDWA . inContext dnfExercise

-- QuickCheck property to monitor the number of steps needed 
-- to normalize a random proposition (30-40% is ok)
{-
testGen :: Property
testGen = forAll generateLogic $ \p -> 
   let n = steps p
   in countEquivalences p <= 2 ==> label (show (n >= 4 && n <= 12)) True
   
testme :: IO ()
testme = quickCheck testGen 

start = ((r :<->: p) :||: (q :->: s)) :&&: (Not s :<->: (p :||: r))
 where
  (p, q, r, s) = (Var "p", Var "q", Var "r", Var "s")

go = derivation . emptyState dnfExercise
-}