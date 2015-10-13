-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A collection of strategy combinators: all lifted to work on different
-- data types
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Combinators where

import Data.Array
import Data.Graph
import Data.List ((\\))
import Ideas.Common.CyclicTree hiding (label)
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Configuration
import Ideas.Common.Strategy.Process
import Ideas.Common.Utils (fst3)
import Prelude hiding (not, repeat, fail, sequence)
import qualified Ideas.Common.Strategy.Choice as Choice
import qualified Ideas.Common.Strategy.Derived as Derived
import qualified Ideas.Common.Strategy.Sequence as Sequence
import qualified Prelude

-----------------------------------------------------------
--- Strategy combinators

-- Basic combinators --------------------------------------

infixr 2 .%., .@.
infixr 3 .|.
infixr 4 ./., |>
infixr 5 .*., !~>

-- | Put two strategies in sequence (first do this, then do that)
(.*.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(.*.) = liftS2 (Sequence..*.)

-- | Choose between the two strategies (either do this or do that)
(.|.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(.|.) = liftS2 (Choice..|.)

-- | Interleave two strategies
(.%.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s .%. t = interleave [toStrategy s, toStrategy t]

-- | Alternate two strategies
(.@.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(.@.) = liftS2 $ useCombinator $ combinator2 "alternate" (Derived.<@>)

-- | Prefixing a basic rule to a strategy atomically
(!~>) :: IsStrategy f => Rule a -> f a -> Strategy a
(!~>) = liftS2 $ useCombinator $ combinator2 "atomicprefix" (Derived.!*>)

-- | Initial prefixes (allows the strategy to stop succesfully at any time)
inits :: IsStrategy f => f a -> Strategy a
inits = liftS $ useCombinator $ combinator1 "inits" Derived.inits

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = Sequence.done

-- | The strategy that always fails
fail :: Strategy a
fail = Choice.empty

-- | Makes a strategy atomic (w.r.t. parallel composition)
atomic :: IsStrategy f => f a -> Strategy a
atomic = liftS $ useCombinator $ combinator1 "atomic" Derived.atomic

-- | Puts a list of strategies into a sequence
sequence :: IsStrategy f => [f a] -> Strategy a
sequence = Sequence.sequence . map toStrategy

-- | Combines a list of alternative strategies
choice :: IsStrategy f => [f a] -> Strategy a
choice = Choice.choice . map toStrategy

-- | Merges a list of strategies (in parallel)
interleave :: IsStrategy f => [f a] -> Strategy a
interleave = liftSn $ useCombinator $ combinatorA interleaveId Derived.interleave

noInterleaving :: IsStrategy f => f a -> Strategy a
noInterleaving = onStrategyTree (replaceNode f)
 where
   f d = if getId d == interleaveId  
         then useDef (combinator "sequence" Sequence.sequence) -- fix me
         else node d

interleaveId :: Id
interleaveId = newId "interleave"
   
-- | Allows all permutations of the list
permute :: IsStrategy f => [f a] -> Strategy a
permute = liftSn $ useCombinator $ combinatorA "permute" Derived.permute

-- EBNF combinators --------------------------------------

-- | Repeat a strategy zero or more times (non-greedy)
many :: IsStrategy f => f a -> Strategy a
many = liftS $ useCombinator $ combinator1 "many" Derived.many

-- | Apply a certain strategy at least once (non-greedy)
many1 :: IsStrategy f => f a -> Strategy a
many1 = liftS $ useCombinator $ combinator1 "many1" Derived.many1

-- | Apply a strategy a certain number of times
replicate :: IsStrategy f => Int -> f a -> Strategy a
replicate n = liftS $ useCombinator $ combinator1 ("replicate" # show n) (Derived.replicate n)

-- | Apply a certain strategy or do nothing (non-greedy)
option :: IsStrategy f => f a -> Strategy a
option = liftS $ useCombinator $ combinator1 "option" Derived.option

-- Negation and greedy combinators ----------------------

-- | Checks whether a predicate holds for the current term. The
--   check is considered to be a minor step.
check :: (a -> Bool) -> Strategy a
check = toStrategy . checkRule "check"

-- | Check whether or not the argument strategy cannot be applied: the result
--   strategy only succeeds if this is not the case (otherwise it fails).
not :: IsStrategy f => f a -> Strategy a
not = liftS $ useCombinator $ combinator1 "not" $ \x -> 
   Sequence.single $ checkRule "core.not" $ null . runProcess x

-- | Repeat a strategy zero or more times (greedy version of 'many')
repeat :: IsStrategy f => f a -> Strategy a
repeat = liftS $ useCombinator $ combinator1 "repeat" Derived.repeat
   
-- | Apply a certain strategy at least once (greedy version of 'many1')
repeat1 :: IsStrategy f => f a -> Strategy a
repeat1 = liftS $ useCombinator $ combinator1 "repeat1" Derived.repeat1

-- | Apply a certain strategy if this is possible (greedy version of 'option')
try :: IsStrategy f => f a -> Strategy a
try = liftS $ useCombinator $ combinator1 "try" Derived.try

-- | Choose between the two strategies, with a preference for steps from the
-- left hand-side strategy.
(./.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(./.) = liftS2 (Choice../.)

-- | Left-biased choice: if the left-operand strategy can be applied, do so. Otherwise,
--   try the right-operand strategy
(|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(|>) = liftS2 (Choice.|>)
-- s |> t = s <|> (not s .*. t)

-- | Repeat the strategy as long as the predicate holds
while :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
while p s = repeat (check p .*. s)

-- | Repeat the strategy until the predicate holds
until :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
until p = while (Prelude.not . p)

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = liftSn $ useCombinator $ combinator "exhaustive" Derived.exhaustive

-- | Apply a strategy at least once, but collapse into a single step
multi :: (IsId l, IsStrategy f) => l -> f a -> Strategy a
multi l = collapse . label l . repeat1

-- Graph to strategy ----------------------

type DependencyGraph node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

-- | Create a strategy from a dependency graph with strategies as nodes
-- Does not check for cycles
dependencyGraph:: IsStrategy f => DependencyGraph (f a) key -> Strategy a
dependencyGraph (graph, vertex2data, _) = g2s []
    where
        g2s seen
            | null reachables   = succeed
            | otherwise         = choice $ map makePath reachables
            where
               reachables      = filter isReachable $ vertices graph \\ seen
               isReachable     = null . (\\ seen) . (graph!)
               makePath vertex = (fst3 . vertex2data) vertex .*. g2s (vertex:seen)