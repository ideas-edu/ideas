{-# LANGUAGE FlexibleContexts #-}
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
-- This module defines extra combinators.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Derived
   ( -- * General combinators
     permute, many, many1, replicate, option, try
   , repeat, repeat1, exhaustive
     -- * Process-specific combinators
   , atomic, (<%>), interleave
   , (<@>), (!*>), inits, filterP, hide
   ) where

import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Symbol
import Prelude hiding (sequence, replicate, repeat)
import qualified Prelude

split :: AtomicSymbol a => (a -> Bool) -> (Process a -> Process a) -> Process a -> Process a
split skipCond cont = rec (0 :: Int)
 where
   rec n = withMenu op empty
    where
      op a = a ~> rest
       where
         next | a == atomicOpen  = n+1
              | a == atomicClose = n-1
              | otherwise        = n
         rest | skipCond a       = rec next
              | next > 0         = rec next
              | otherwise        = cont

-- atomic prefix
(!*>) :: AtomicSymbol a => Process a -> Process a -> Process a
a !*> p = atomicOpen ~> a .*. withMenu op (single atomicClose) p
 where
   op b q
      | b == atomicOpen = q
      | otherwise       = b ~> atomicClose ~> q

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = fold (\a q -> if cond a then a ~> q else empty) done

hide :: (a -> Bool) -> Process a -> Process a
hide cond = fold (\a q -> if cond a then a ~> q else q) done

atomic :: AtomicSymbol a => Process a -> Process a
atomic p = atomicOpen ~> (p .*. single atomicClose)

interleave :: (AtomicSymbol a, LabelSymbol a) => [Process a] -> Process a
interleave xs = if null xs then done else foldr1 (<%>) xs

-- interleaving
(<%>) :: (AtomicSymbol a, LabelSymbol a) => Process a -> Process a -> Process a
p <%> q =
   bothAreDone p q .|. ((p %>> q) .|. (q %>> p))
 where
   bothAreDone = withMenu stop2 . withMenu stop2 done
   stop2 _ _   = empty
   r %>> s     = split isEnterSymbol (<%> s) r

-- | Allows all permutations of the list
permute :: (Choice a, Sequence a) => [a] -> a
permute as
   | null as   = done
   | otherwise = choice [ s .*. permute ys | (s, ys) <- pickOne as ]
 where
   pickOne :: [a] -> [(a, [a])]
   pickOne []     = []
   pickOne (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- pickOne xs ]

-- Alternate combinator
(<@>) :: AtomicSymbol a => Process a -> Process a -> Process a
p <@> q = bothOk p q .|. (p @>> q)
 where
   bothOk  = withMenu (\_ _ -> empty) done
   r @>> s = split (const False) (s <@>) r

inits :: AtomicSymbol a => Process a -> Process a
inits p = done .|. split (const False) inits p

many :: (Sequence a, Fix a, Choice a) => a -> a
many s = fix $ \x -> done .|. (s .*. x)

many1 :: (Sequence a, Fix a, Choice a) => a -> a
many1 s = s .*. many s

replicate :: Sequence a => Int -> a -> a
replicate n = sequence . Prelude.replicate n

-- | Apply a certain strategy or do nothing (non-greedy)
option :: (Choice a, Sequence a) => a -> a
option s = s .|. done

-- | Apply a certain strategy if this is possible (greedy version of 'option')
try :: (Choice a, Sequence a) => a -> a
try s = s |> done

-- | Repeat a strategy zero or more times (greedy version of 'many')
repeat :: (Sequence a, Fix a, Choice a) => a -> a
repeat s = fix $ \x -> try (s .*. x)

-- | Apply a certain strategy at least once (greedy version of 'many1')
repeat1 :: (Sequence a, Fix a, Choice a) => a -> a
repeat1 s = s .*. repeat s

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: (Sequence a, Fix a, Choice a) => [a] -> a
exhaustive = repeat . choice

---------------------------------------------------------------------------