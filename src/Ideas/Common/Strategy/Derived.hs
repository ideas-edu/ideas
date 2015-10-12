{-# LANGUAGE FlexibleContexts #-}
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
-----------------------------------------------------------------------------
--  $Id: Sequential.hs 6612 2014-06-12 07:57:59Z bastiaan $

module Ideas.Common.Strategy.Derived
   ( AtomicSymbol(..), LabelSymbol(..)
   , atomic, (<%>), interleave, permute, concurrent, (<@>), (!*>), inits
   , filterP, hide
   ) where

import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Step

useFirst :: Choice b => (a -> Process a -> b) -> b -> Process a -> b
useFirst op e = menuFirst op e

split :: (AtomicSymbol a, Choice b)
      => (Process a -> Process a -> b) -> b -> Process a -> b         
split op = split2 (op . single) op

-- Specialized version of split that also takes an operator for the special case
-- that the left part of the split is a single symbol. 
split2 :: (AtomicSymbol a, Choice b)
       => (a -> Process a -> b) -> (Process a -> Process a -> b) -> b -> Process a -> b         
split2 op1 op2 = useFirst f
 where
   f a | a == atomicOpen = rec (op2 . (a ~>)) 1
       | otherwise       = op1 a
         
   rec acc n
      | n == 0    = acc done
      | otherwise = useFirst g empty
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

   stepBoth  = useFirst stop2 . useFirst stop2 done
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
   bothOk = useFirst (\_ _ -> empty) done

inits :: AtomicSymbol a => Process a -> Process a
inits = rec
 where
   rec p = done .|. split op empty p
   op x  = (x .*.) . rec

---------------------------------------------------------------------------