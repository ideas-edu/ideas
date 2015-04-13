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
   , AtomicSymbol(..), atomic, concurrent, (<@>)
   ) where

import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence

useFirst :: Choice f => (a -> Process a -> f b) -> f b -> Process a -> f b
useFirst op e = onMenu (menuItem e op) . menu

split :: (IsProcess f, AtomicSymbol a) => Process a -> Menu (Either a (f a), Process a)
split p = onMenu (menuItem empty f) (menu p)
 where
   f a s | a == atomicOpen = fmap (make a) (rec 1 s)
         | otherwise = single (Left a, s)
         
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

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = fold (\a q -> if cond a then a ~> q else empty) done

hide :: (a -> Bool) -> Process a -> Process a
hide cond = fold (\a q -> if cond a then a ~> q else q) done

class Eq a => AtomicSymbol a where
   atomicOpen, atomicClose :: a

atomic :: (IsProcess f, AtomicSymbol a) => f a -> f a
atomic p = atomicOpen ~> (p <*> single atomicClose)

concurrent :: (IsProcess f, AtomicSymbol a) => (a -> Bool) -> f a -> f a -> f a
concurrent switch x y = normal (toProcess x) (toProcess y)
 where
   normal p q = stepBoth q p <|> (stepRight q p <|> stepRight p q)

   stepBoth  = useFirst stop2 . useFirst stop2 done
   stop2 _ _ = empty

   stepRight p q = onMenu op (split (toProcess q))
    where
      op (Left a, q2)
         | switch a  = a ~> normal p q2
         | otherwise = a ~> stepRight p q2
      op (Right q1, q2) = q1 <*> normal p q2

-- Alternate combinator
(<@>) :: IsProcess f => f a -> f a -> f a
p0 <@> q0 = rec (toProcess q0) (toProcess p0)
 where
   rec q  = useFirst (\a r -> a ~> rec r q) (bothOk q)
   bothOk = useFirst (\_ _ -> empty) done

---------------------------------------------------------------------------