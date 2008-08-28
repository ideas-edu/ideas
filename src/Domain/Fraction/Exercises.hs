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
import Control.Monad
import Data.Maybe

simplExercise :: Exercise (Context Frac)
simplExercise = standard
   { shortTitle    = "Simplifying fractions" 
   , parser        = \s -> case parseFrac s of
                              (p, [])   -> Right (inContext p)
                              (p, msgs) -> Left  (text (show msgs))
   , prettyPrinter = ppFrac . fromContext
   , equivalence   = \x y -> fromContext x ~= fromContext y
   , equality      = \x y -> fromContext x == fromContext y
   , finalProperty = \x -> fromContext x == fromContext (fromJust $ apply toSimple x)
   , ruleset       = map liftRuleToContext fracRules
   , strategy      = toSimple
   , generator     = liftM inContext generateFrac
   , suitableTerm  = \t -> not $ finalProperty simplExercise t && isJust (nf (fromContext t))
   }
 where
   standard :: Exercise (Context Frac)
   standard = makeExercise