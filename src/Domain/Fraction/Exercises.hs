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
   , ordering      = compare
   , generator     = generateFrac
   , suitableTerm  = \t -> not $ finalProperty simplExercise t && isJust (nf t)
   }
 where
   standard :: Exercise Frac
   standard = makeExercise

{-
addFracExercise :: Exercise Frac
addFracExercise =  simplExercise
   { identifier    = "addfraction"
   , domain        = "math"
   , description   = "Add fractions" 
   , status        = Experimental
   , ruleset       = map liftRuleToContext fracRules
   , strategy      = addFractions
   , generator     = generateWith config
   , suitableTerm  = \t -> not $ finalProperty simplExercise t && isJust (nf t)
   }
 where
   config = { maxSize   = 2
            , range     = (-6,6)
            , diffVars  = 1
            , freqConst = 0
            , freqVar   = 0
            , freqMul   = 0
            , freqDiv   = 0
            , freqAdd   = 3
            , freqSub   = 3
            , freqNeg   = 0
            }
-}
