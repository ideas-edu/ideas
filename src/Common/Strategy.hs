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
-- A strategy is a context-free grammar with rules as symbols. Strategies can be 
-- labeled with strings. A type class is introduced to lift all the combinators
-- that work on strategies, only to prevent that you have to insert these lifting
-- functions yourself.
--
-----------------------------------------------------------------------------
module Common.Strategy 
   ( -- * Data types and type classes
     Strategy, LabeledStrategy, strategyName
   , IsStrategy(..)
     -- * Running strategies
   , fullDerivationTree, derivationTree
     -- * Strategy combinators
     -- ** Basic combinators
   , (<*>), (<|>), succeed, fail, label, sequence, alternatives -- <||>
     -- ** EBNF combinators
   , many, many1, replicate, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), exhaustive
     -- ** Traversal combinators
   , fix, once, somewhere, topDown, bottomUp
     -- * Configuration combinators
   , module Common.Strategy.Configuration
     -- * Strategy locations
   , StrategyLocation, topLocation, nextLocation, downLocation
   , locationDepth
   , subTaskLocation, nextTaskLocation, parseStrategyLocation
   , StrategyOrRule, subStrategy, strategyLocations
   , mapRules, mapRulesS, rulesInStrategy, cleanUpStrategy
     -- * Prefixes
   , Prefix, emptyPrefix, makePrefix, prefixTree, Step(..)
   , prefixToSteps, stepsToRules, lastStepInPrefix
   ) where

import Common.Strategy.Abstract
import Common.Strategy.Combinators
import Common.Strategy.Prefix
import Common.Strategy.Location
import Common.Strategy.Configuration
import Common.Strategy.Representation () -- only XML instance

import qualified Prelude