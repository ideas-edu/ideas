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
import Domain.Logic.BuggyRules
import Domain.Logic.Difference
import Common.Apply
import qualified Common.Grammar as RE
import Common.Derivation
import Common.Exercise
import Common.Context
import Common.Strategy
import Common.Transformation
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
   , newDifference  = \_ -> differenceEqual eqLogic
   , testGenerator  = Just (restrictGenerator suitable generateLogic)
   , randomExercise = useGenerator (const True) logicExercise
   }

-- Direct support for unicode characters
dnfUnicodeExercise :: Exercise SLogic
dnfUnicodeExercise = dnfExercise
   { description   = description dnfExercise ++ " (unicode support)"
   , exerciseCode  = makeCode "logic" "dnf-unicode"
   , parser        = either Left (Right . fromRanged) . parseLogicUnicodePars
   , prettyPrinter = ppLogicUnicodePars
   }

logicExercise :: Int -> Gen SLogic
logicExercise n = 
   let (gen, (minStep, maxStep)) 
          | n == 1    = generateLevel Easy
          | n == 3    = generateLevel Difficult 
          | otherwise = generateLevel Normal 
       ok p = let n = steps (maxStep+1) p
              in countEquivalences p <= 2 && n >= minStep && n <= maxStep
   in restrictGenerator ok gen

suitable :: SLogic -> Bool
suitable = (<=2) . countEquivalences

steps :: Int -> SLogic -> Int
steps i = stepsMax i 
        . filterIntermediates isMajorRule 
        . makeDerivation dnfStrategyDWA 
        . inContext

-- Todo: move these two derivation functions to Common.Strategy
makeDerivation :: LabeledStrategy a -> a -> DerivationTree (Rule a) a
makeDerivation = grammarDerivation . noLabels . unlabel

grammarDerivation :: RE.Grammar (Rule a) -> a -> DerivationTree (Rule a) a
grammarDerivation s a = addBranches list (emptyNode a (RE.empty s))
 where
   list = [ (f, grammarDerivation rest b) 
          | (f, rest) <- RE.firsts s
          , b <- applyAll f a 
          ]

-- QuickCheck property to monitor the number of steps needed 
-- to normalize a random proposition (30-40% is ok)
{-
testGen :: Property
testGen = forAll generateLogic $ \p -> 
   let n = steps p
   in countEquivalences p <= 2 ==> label (show (n >= 4 && n <= 12)) True
   
testme :: IO ()
testme = quickCheck testGen -}