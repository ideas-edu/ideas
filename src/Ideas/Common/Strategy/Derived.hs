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
   ( filterP, hide
   , AtomicSymbol(..), LabelSymbol(..)
   , atomic, (<%>), interleave, permute, concurrent, (<@>), (!*>), inits
   ) where

import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence

useFirst :: Choice b => (a -> Process a -> b) -> b -> Process a -> b
useFirst op e = onMenu op e . menu

split :: (AtomicSymbol a, Choice b)
      => (Builder a -> Process a -> b) -> b -> Process a -> b         
split op = split2 (op . single) op

-- Specialized version of split that also takes an operator for the special case
-- that the left part of the split is a single symbol. 
split2 :: (AtomicSymbol a, Choice b)
       => (a -> Process a -> b) -> (Builder a -> Process a -> b) -> b -> Process a -> b         
split2 op1 op2 = useFirst f
 where
   f a | a == atomicOpen = rec (op2 . (a ~>)) 1
       | otherwise       = op1 a
         
   rec acc n
      | n == 0    = acc done
      | otherwise = onMenu g empty . menu
    where
      g a = rec (acc . (a ~>)) (pm a n)

   pm :: AtomicSymbol a => a -> Int -> Int
   pm a | a == atomicOpen  = succ
        | a == atomicClose = pred
        | otherwise        = id

-- atomic prefix
(!*>) :: AtomicSymbol a => Builder a -> Builder a -> Builder a
a !*> p = split op (atomic a) (fromBuilder p)
 where
   op b q = atomic (a .*. b) .*. toBuilder q

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = fold (\a q -> if cond a then a ~> q else empty) done

hide :: (a -> Bool) -> Process a -> Process a
hide cond = fold (\a q -> if cond a then a ~> q else q) done

class Eq a => AtomicSymbol a where
   atomicOpen, atomicClose :: a

class Eq a => LabelSymbol a where
   isEnter :: a -> Bool

atomic :: AtomicSymbol a => Builder a -> Builder a
atomic p = atomicOpen ~> (p .*. single atomicClose)

interleave :: (AtomicSymbol a, LabelSymbol a) => [Builder a] -> Builder a
interleave xs = if null xs then done else foldr1 (<%>) xs

(<%>) :: (AtomicSymbol a, LabelSymbol a) => Builder a -> Builder a -> Builder a
(<%>) = concurrent (not . isEnter)

concurrent :: AtomicSymbol a => (a -> Bool) -> Builder a -> Builder a -> Builder a
concurrent switch x y = normal (fromBuilder x) (fromBuilder y)
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
(<@>) :: AtomicSymbol a => Builder a -> Builder a -> Builder a
p0 <@> q0 = rec (fromBuilder q0) (fromBuilder p0)
 where
   rec q  = let op b r = b .*. rec r q
            in split op (bothOk q)
   bothOk = useFirst (\_ _ -> empty) done

inits :: AtomicSymbol a => Builder a -> Builder a
inits = rec . fromBuilder
 where
   rec p = done .|. split op empty p
   op x  = (x .*.) . rec

---------------------------------------------------------------------------