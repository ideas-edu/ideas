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

import Domain.Fraction.Zipper
import Domain.Fraction.Generator
import Domain.Fraction.Frac
import Domain.Fraction.Strategies
import Domain.Fraction.Parser
import Domain.Fraction.Rules

import Common.Apply
import Common.Exercise
import Common.Transformation
import Control.Monad
import System.Random
import Data.Maybe

simplExercise :: Exercise FracInContext
simplExercise = Exercise
   { shortTitle    = "Simplifying fractions" 
   , parser        = \s -> case parseFrac s of
                              (p, [])   -> Right (inContext p)
                              (p, msgs) -> Left  (text (show msgs), Just (inContext p))
   , prettyPrinter = ppFracPars . noContext
   , equivalence   = \x y -> noContext x ~= noContext y
   , equality      = \x y -> noContext x == noContext y
   , finalProperty = \x -> noContext x == noContext (fromJust $ apply toSimple x)
   , ruleset       = map liftFracRule fracRules
   , strategy      = toSimple
   , generator     = liftM inContext generateFrac
   , suitableTerm  = \t -> not $ finalProperty simplExercise t && nf (noContext t) /= Nothing
   , configuration = defaultConfiguration
   }
