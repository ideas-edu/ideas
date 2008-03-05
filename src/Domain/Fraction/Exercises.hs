-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)e
--
-----------------------------------------------------------------------------
module Domain.Fraction.Exercises where

import Domain.Fraction.Generator
import Domain.Fraction.Frac
import Domain.Fraction.Strategies
import Domain.Fraction.Parser
import Domain.Fraction.Rules

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Transformation
import Control.Monad
import System.Random
import Data.Maybe

simplExercise :: Exercise (Context Frac)
simplExercise = Exercise
   { shortTitle    = "Simplifying fractions" 
   , parser        = \s -> case parseFrac s of
                              (p, [])   -> Right (inContext p)
                              (p, msgs) -> Left  (text (show msgs), Just (inContext p))
   , prettyPrinter = ppFracPars . fromContext
   , equivalence   = \x y -> fromContext x ~= fromContext y
   , equality      = \x y -> fromContext x == fromContext y
   , finalProperty = \x -> fromContext x == fromContext (fromJust $ apply toSimple x)
   , ruleset       = map liftRuleToContext fracRules
   , strategy      = toSimple
   , generator     = liftM inContext generateFrac
   , suitableTerm  = \t -> not $ finalProperty simplExercise t && nf (fromContext t) /= Nothing
   , configuration = defaultConfiguration
   }
