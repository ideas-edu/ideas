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
-- A strategy is a context-free grammar with rules as symbols. Strategies can be 
-- labeled with strings. A type class is introduced to lift all the combinators
-- that work on strategies, only to prevent that you have to insert these lifting
-- functions yourself.
--
-----------------------------------------------------------------------------
module Common.Strategy 
   ( -- * Data types and type classes
     Strategy, LabeledStrategy
   , IsStrategy(..)
     -- * Running strategies
   , fullDerivationTree, derivationTree
     -- * Strategy combinators
     -- ** Basic combinators
   , (<*>), (<|>), (<%>), succeed, fail, atomic, label
   , sequence, alternatives, interleave, permute
     -- ** EBNF combinators
   , many, many1, replicate, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), exhaustive
   , while, until, multi
     -- ** Traversal combinators
   , fix, once, somewhere, topDown, bottomUp
   , onceWith, somewhereWith
     -- * Configuration combinators
   , module Common.Strategy.Configuration
     -- * Strategy locations
   , strategyLocations, subStrategy
   , subTaskLocation, nextTaskLocation
     -- * Prefixes
   , Prefix, emptyPrefix, makePrefix, prefixTree, Step(..)
   , prefixToSteps, stepsToRules, lastStepInPrefix
     -- * Misc
   , cleanUpStrategy, rulesInStrategy, mapRules, mapRulesS
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Combinators
import Common.Strategy.Prefix
import Common.Strategy.Location
import Common.Strategy.Configuration
import Common.Strategy.Parsing
import Prelude ()