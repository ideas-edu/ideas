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

useFirst :: Choice f => (a -> Process a -> f b) -> f b -> Process a -> f b
useFirst op e = onMenu (menuItem e op) . menu

split :: (AtomicSymbol a, Choice f, Sequence f)
      => (Either a (f a) -> Process a -> f b) -> f b -> Process a -> f b         
split op = useFirst f
 where
   f a s | a == atomicOpen = onMenu (uncurry op) $ fmap (make a) (rec 1 s)
         | otherwise       = op (Left a) s
      
   make a (x, y) = (Right (a ~> x), y)
         
   rec n s
      | n == 0    = single (done, s)
      | otherwise = onMenu (menuItem empty g) (menu s)
    where
      g a t = fmap (\(x, y) -> (a ~> x, y)) (rec (pm a n) t)

   pm :: AtomicSymbol a => a -> Int -> Int
   pm a | a == atomicOpen  = succ
        | a == atomicClose = pred
        | otherwise        = id

-- atomic prefix
(!*>) :: (IsProcess f, AtomicSymbol a) => f a -> f a -> f a
a !*> p = split op (atomic a) (toProcess p)
 where
   op (Left b) q   = atomic (a <*> b ~> done) <*> fromProcess q
   op (Right bl) q = atomic (a <*> bl) <*> fromProcess q

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = fold (\a q -> if cond a then a ~> q else empty) done

hide :: (a -> Bool) -> Process a -> Process a
hide cond = fold (\a q -> if cond a then a ~> q else q) done

class Eq a => AtomicSymbol a where
   atomicOpen, atomicClose :: a

class Eq a => LabelSymbol a where
   isEnter :: a -> Bool

atomic :: (IsProcess f, AtomicSymbol a) => f a -> f a
atomic p = atomicOpen ~> (p <*> single atomicClose)

interleave :: (IsProcess f, AtomicSymbol a, LabelSymbol a) => [f a] -> f a
interleave xs = if null xs then done else foldr1 (<%>) xs

(<%>) :: (IsProcess f, AtomicSymbol a, LabelSymbol a) => f a -> f a -> f a
(<%>) = concurrent (not . isEnter)

concurrent :: (IsProcess f, AtomicSymbol a) => (a -> Bool) -> f a -> f a -> f a
concurrent switch x y = normal (toProcess x) (toProcess y)
 where
   normal p q = stepBoth q p <|> (stepRight q p <|> stepRight p q)

   stepBoth  = useFirst stop2 . useFirst stop2 done
   stop2 _ _ = empty

   stepRight p q = split op empty (toProcess q)
    where
      op (Left a) q2
         | switch a  = a ~> normal p q2
         | otherwise = a ~> stepRight p q2
      op (Right q1) q2 = q1 <*> normal p q2

-- | Allows all permutations of the list
permute :: (Choice f, Sequence f) => [f a] -> f a
permute as
   | null as   = done
   | otherwise = choice [ s <*> permute ys | (s, ys) <- pickOne as ]
 where
   pickOne :: [a] -> [(a, [a])]
   pickOne []     = []
   pickOne (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- pickOne xs ]
   
-- Alternate combinator
(<@>) :: (IsProcess f, AtomicSymbol a) => f a -> f a -> f a
p0 <@> q0 = rec (toProcess q0) (toProcess p0)
 where
   rec q  = let f (Left a) r  = a ~>  rec r q
                f (Right b) r = b <*> rec r q
            in split f (bothOk q)
   bothOk = useFirst (\_ _ -> empty) done

inits :: (IsProcess f, AtomicSymbol a) => f a -> f a
inits = rec . toProcess
 where
   rec p = done <|> split op empty p
   op  x = either (~>) (<*>) x . rec

---------------------------------------------------------------------------