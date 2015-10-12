{-# LANGUAGE RankNTypes #-}
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
import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Configuration
import Ideas.Common.CyclicTree hiding (label)
import Ideas.Common.Strategy.Def
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
s .*. t = toStrategy s Sequence..*. toStrategy t

-- | Choose between the two strategies (either do this or do that)
(.|.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s .|. t = toStrategy s Choice..|. toStrategy t

-- | Interleave two strategies
(.%.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(.%.) = liftCore2 (node2 interleaveDef)

-- | Alternate two strategies
(.@.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(.@.) = liftCore2 (node2 alternateDef)

-- Prefixing a basic rule to a strategy (see Ask-Elle)
-- (~>) :: IsStrategy f => Rule a -> f a -> Strategy a
-- a ~> s = a .*. s

-- | Prefixing a basic rule to a strategy atomically
(!~>) :: IsStrategy f => Rule a -> f a -> Strategy a
a !~> s = liftCore (node2 atomicPrefixDef (leaf a)) s

-- | Initial prefixes (allows the strategy to stop succesfully at any time)
inits :: IsStrategy f => f a -> Strategy a
inits = liftCore (node1 initsDef)

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = Sequence.done

-- | The strategy that always fails
fail :: Strategy a
fail = Choice.empty

-- | Makes a strategy atomic (w.r.t. parallel composition)
atomic :: IsStrategy f => f a -> Strategy a
atomic = liftCore (node1 atomicDef)

-- | Puts a list of strategies into a sequence
sequence :: IsStrategy f => [f a] -> Strategy a
sequence = Sequence.sequence . map toStrategy

-- | Combines a list of alternative strategies
alternatives :: IsStrategy f => [f a] -> Strategy a
alternatives = Choice.choice . map toStrategy

-- | Merges a list of strategies (in parallel)
interleave :: IsStrategy f => [f a] -> Strategy a
interleave = liftCoreN (node interleaveDef)

noInterleaving :: IsStrategy f => f a -> Strategy a
noInterleaving = liftCore (mapFirst f)
 where
   f d | d == interleaveDef = associativeDef "sequence" Sequence.sequence -- fix me
       | otherwise          = d
   
-- | Allows all permutations of the list
permute :: IsStrategy f => [f a] -> Strategy a
permute = liftCoreN (node permuteDef)

-- EBNF combinators --------------------------------------

-- | Repeat a strategy zero or more times (non-greedy)
many :: IsStrategy f => f a -> Strategy a
many = liftCore manyCore

manyCore :: Core a -> Core a
manyCore = node1 manyDef
   
manyDef :: Def
manyDef = makeDef1 "many" $
   let f x = Sequence.done Choice..|. (x Sequence..*. f x)
   in f

-- | Apply a certain strategy at least once (non-greedy)
many1 :: IsStrategy f => f a -> Strategy a
many1 s = s .*. many s

-- | Apply a strategy a certain number of times
replicate :: IsStrategy f => Int -> f a -> Strategy a
replicate n = sequence . Prelude.replicate n

-- | Apply a certain strategy or do nothing (non-greedy)
option :: IsStrategy f => f a -> Strategy a
option s = s .|. succeed

-- Negation and greedy combinators ----------------------

-- | Checks whether a predicate holds for the current term. The
--   check is considered to be a minor step.
check :: (a -> Bool) -> Strategy a
check = toStrategy . checkRule "check"

-- | Check whether or not the argument strategy cannot be applied: the result
--   strategy only succeeds if this is not the case (otherwise it fails).
not :: IsStrategy f => f a -> Strategy a
not = liftCore (node1 notDef)

-- | Repeat a strategy zero or more times (greedy version of 'many')
repeat :: IsStrategy f => f a -> Strategy a
repeat = liftCore repeatCore

repeatCore :: Core a -> Core a
repeatCore = node1 repeatDef
   
repeatDef :: Def
repeatDef = makeDef1 "repeat" $ 
   let f x = (x Sequence..*. f x) Choice.|> Sequence.done
   in f
   
-- | Apply a certain strategy at least once (greedy version of 'many1')
repeat1 :: IsStrategy f => f a -> Strategy a
repeat1 s = s .*. repeat s

-- | Apply a certain strategy if this is possible (greedy version of 'option')
try :: IsStrategy f => f a -> Strategy a
try s = s |> succeed

-- | Choose between the two strategies, with a preference for steps from the
-- left hand-side strategy.
(./.) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s ./. t = toStrategy s Choice../. toStrategy t

-- | Left-biased choice: if the left-operand strategy can be applied, do so. Otherwise,
--   try the right-operand strategy
(|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s |> t = toStrategy s Choice.|> toStrategy t
-- s |> t = s <|> (not s .*. t)

-- | Repeat the strategy as long as the predicate holds
while :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
while p s = repeat (check p .*. s)

-- | Repeat the strategy until the predicate holds
until :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
until p = while (Prelude.not . p)

-- | Apply a strategy at least once, but collapse into a single step
multi :: (IsId l, IsStrategy f) => l -> f a -> Strategy a
multi l = collapse . label l . repeat1

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = repeat . alternatives

remove :: IsStrategy f => f a -> Strategy a
remove = liftCore removeCore

collapse :: IsStrategy f => f a -> Strategy a
collapse = liftCore collapseCore

hide :: IsStrategy f => f a -> Strategy a
hide = liftCore hideCore

---------------------------------------------------------------------------

notDef :: Def
notDef = makeDef1 "not" $ \x -> 
   Sequence.single $ checkRule "core.not" $ null . runProcess x

interleaveDef :: Def
interleaveDef = associativeDef "interleave" Derived.interleave

permuteDef :: Def
permuteDef = associativeDef "permute" Derived.permute

alternateDef :: Def
alternateDef = makeDef2 "alternate" (Derived.<@>)

atomicDef :: Def
atomicDef = makeDef1 "atomic" Derived.atomic

atomicPrefixDef :: Def
atomicPrefixDef = makeDef2 "atomicprefix" (Derived.!*>)

initsDef :: Def
initsDef = makeDef1 "inits" Derived.inits

-- Graph to strategy ----------------------

type DependencyGraph node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

-- | Create a strategy from a dependency graph with strategies as nodes
-- Does not check for cycles
dependencyGraph:: IsStrategy f => DependencyGraph (f a) key -> Strategy a
dependencyGraph (graph, vertex2data, _) = g2s []
    where
        g2s seen
            | null reachables   = succeed
            | otherwise         = alternatives $ map makePath reachables
            where
               reachables      = filter isReachable $ vertices graph \\ seen
               isReachable     = null . (\\ seen) . (graph!)
               makePath vertex = (fst3 . vertex2data) vertex .*. g2s (vertex:seen)