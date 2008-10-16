-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
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
import Data.Maybe

simplExercise :: Exercise Frac
simplExercise = standard
   { identifier    = "fraction"
   , domain        = "math"
   , description   = "Simplifying fractions" 
   , status        = Experimental
   , parser        = parseFrac
   , prettyPrinter = ppFrac
   , equivalence   = (~=)
   , equality      = (==)
   , finalProperty = \x -> x == (fromContext $ fromJust $ apply toSimple $ inContext x)
   , ruleset       = map liftRuleToContext fracRules
   , strategy      = toSimple
   , generator     = generateFrac
   , suitableTerm  = \t -> not $ finalProperty simplExercise t && isJust (nf t)
   }
 where
   standard :: Exercise (Context Frac)
   standard = makeExercise