{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
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
   , atomic, (<%>), interleave, concurrent
   , (<@>), (!*>), inits, filterP, hide
   ) where

import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Symbol
import Prelude hiding (sequence, replicate, repeat)
import qualified Prelude

split :: (AtomicSymbol a, Choice b)
      => (Process a -> Process a -> b) -> b -> Process a -> b
split op = split2 (op . single) op

-- Specialized version of split that also takes an operator for the special case
-- that the left part of the split is a single symbol.
split2 :: (AtomicSymbol a, Choice b)
       => (a -> Process a -> b) -> (Process a -> Process a -> b) -> b -> Process a -> b
split2 op1 op2 = withMenu f
 where
   f a | a == atomicOpen = rec (op2 . (a ~>)) 1
       | otherwise       = op1 a

   rec acc n
      | n == 0    = acc done
      | otherwise = withMenu g empty
    where
      g a = rec (acc . (a ~>)) (pm a n)

   pm :: AtomicSymbol a => a -> Int -> Int
   pm a | a == atomicOpen  = succ
        | a == atomicClose = pred
        | otherwise        = id

-- atomic prefix
(!*>) :: AtomicSymbol a => Process a -> Process a -> Process a
a !*> p = split op (atomic a) p
 where
   op b q = atomic (a .*. b) .*. q

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = fold (\a q -> if cond a then a ~> q else empty) done

hide :: (a -> Bool) -> Process a -> Process a
hide cond = fold (\a q -> if cond a then a ~> q else q) done

atomic :: AtomicSymbol a => Process a -> Process a
atomic p = atomicOpen ~> (p .*. single atomicClose)

interleave :: (AtomicSymbol a, LabelSymbol a) => [Process a] -> Process a
interleave xs = if null xs then done else foldr1 (<%>) xs

(<%>) :: (AtomicSymbol a, LabelSymbol a) => Process a -> Process a -> Process a
(<%>) = concurrent (not . isEnterSymbol)

concurrent :: AtomicSymbol a => (a -> Bool) -> Process a -> Process a -> Process a
concurrent switch = normal
 where
   normal p q = stepBoth q p .|. (stepRight q p .|. stepRight p q)

   stepBoth  = withMenu stop2 . withMenu stop2 done
   stop2 _ _ = empty

   stepRight p = split2 op1 op2 empty
    where
      op1 a q2
         | switch a  = a ~> normal p q2
         | otherwise = a ~> stepRight p q2
      op2 q1 q2 = q1 .*. normal p q2

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
p0 <@> q0 = rec q0 p0
 where
   rec q  = let op b r = b .*. rec r q
            in split op (bothOk q)
   bothOk = withMenu (\_ _ -> empty) done

inits :: AtomicSymbol a => Process a -> Process a
inits = rec
 where
   rec p = done .|. split op empty p
   op x  = (x .*.) . rec

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
