-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A strategy is a context-free grammar with rules as symbols. Strategies can be
-- labeled with strings. The type class 'IsStrategy' is introduced to lift
-- functions and combinators that work on strategies to also accept rules and
-- labeled strategies. This module re-exports the most important functionality
-- of the underlying modules.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy
   ( -- * Data types and type classes
     Strategy, LabeledStrategy
   , IsStrategy(..)
     -- * Running strategies
   , derivationList
     -- * Strategy combinators
     -- ** Basic combinators
   , (.*.), (.|.), (.%.), (.@.), (!~>)
   , succeed, fail, atomic, label, inits
   , sequence, choice, preference, orelse
   , alternatives, interleave, permute
     -- ** EBNF combinators
   , many, many1, replicate, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), (./.)
   , exhaustive, while, until, dynamic
     -- ** Graph
   , DependencyGraph, dependencyGraph
     -- ** Traversal combinators
   , module Ideas.Common.Strategy.Traversal
     -- * Configuration combinators
   , module Ideas.Common.Strategy.Configuration
     -- * Strategy locations
   , strategyLocations, checkLocation
   , subTaskLocation, nextTaskLocation
     -- * Prefixes
   , Prefix, emptyPrefix, noPrefix
   , replayPath, replayPaths, replayStrategy
   , Path, emptyPath, readPath, readPaths
   , prefixPaths, majorPrefix, isEmptyPrefix
     -- * Misc
   , cleanUpStrategy, cleanUpStrategyAfter
   , rulesInStrategy
   ) where

import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Combinators
import Ideas.Common.Strategy.Configuration
import Ideas.Common.Strategy.Legacy (alternatives)
import Ideas.Common.Strategy.Location
import Ideas.Common.Strategy.Prefix
import Ideas.Common.Strategy.Traversal hiding (full, spine, stop, once)
import Prelude ()