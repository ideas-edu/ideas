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
-- A type class for expressing choice, preference, and left-biased choice.
-- The 'Menu' datatype implements the type class by keeping all the
-- alternatives.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Choice
   ( -- * Choice type class
     Choice(..)
     -- * Menu data type
   , Menu, (|->), doneMenu, eqMenuBy
     -- * Queries
   , elems, bests, bestsOrdered
   , isEmpty, hasDone, getByIndex, cut
     -- * Generalized functions
   , onMenu, onMenuWithIndex
   ) where

import Data.Maybe

infixr 3 .|., ./., |>, :|:, :/:, :|>
infixr 5 |->, :->

------------------------------------------------------------------------
-- Choice type class

-- | Laws: '.|.', './.' '|>' are all associative, and have 'empty' as their
-- unit element.
class Choice a where
   -- | Nothing to choose from.
   empty :: a
   -- | Normal (unbiased) choice.
   (.|.) :: a -> a -> a
   -- | Left-preference.
   (./.) :: a -> a -> a
   -- | Left-biased choice.
   (|>) :: a -> a -> a
   -- | One of the alternatives in a list (unbiased).
   choice     :: [a] -> a
   preference :: [a] -> a
   orelse     :: [a] -> a
   -- default implementation
   choice     xs = if null xs then empty else foldr1 (.|.) xs
   preference xs = if null xs then empty else foldr1 (./.) xs
   orelse     xs = if null xs then empty else foldr1 (|>)  xs

instance Choice [a] where
   empty    = []
   (.|.)    = (++)
   (./.)    = (++)
   xs |> ys = if null xs then ys else xs
   choice   = concat

instance Choice b => Choice (a -> b) where
   empty       = const empty
   (f .|. g) a = f a .|. g a
   (f ./. g) a = f a ./. g a
   (f |> g)  a = f a  |> g a

------------------------------------------------------------------------
-- Menu data type

-- Invariants for the Menu datatype:
-- (Unit) The left-hand side of :|: and :|> cannot be Empty
-- (Asso) :|: and :|> are balanced to the right

-- | A menu offers choices and preferences. It stores singleton bindings (thus
-- acting as a finite map) and one special element ('doneMenu'). It is an
-- instance of the 'Functor' and 'Monad' type classes.
data Menu k a = k :-> a
              | Done
              | Empty
              | Menu k a :|: Menu k a
              | Menu k a :/: Menu k a -- left-preference
              | Menu k a :|> Menu k a -- left-biased

instance (Eq k, Eq a) => Eq (Menu k a) where
   (==) = eqMenuBy (==) (==)

instance Choice (Menu k a) where
   empty  = Empty

   p0 .|. rest = rec p0 -- maintain invariant
    where
     rec Empty     = rest
     rec (p :|: q) = p :|: rec q
     rec p         = case rest of -- strict: also check rhs
                        Empty -> p
                        _     -> p :|: rest

   p0 ./. rest = rec p0 -- maintain invariant
    where
     rec Empty     = rest
     rec (p :/: q) = p :/: rec q
     rec p         = p :/: rest

   p0 |> rest = rec p0 -- maintain invariant
    where
     rec Empty     = rest
     rec (p :|> q) = p :|> rec q
     rec p         = p :|> rest

instance Functor (Menu k) where
   fmap f = rec
    where
      rec (p :|: q) = rec p :|: rec q
      rec (p :/: q) = rec p :/: rec q
      rec (p :|> q) = rec p :|> rec q
      rec (k :-> a) = k :-> f a
      rec Done      = Done
      rec Empty     = Empty

-- | Singleton binding
(|->) :: a -> s -> Menu a s
(|->) = (:->)

-- | Special element for denoting success
doneMenu :: Menu k a
doneMenu = Done

hasDone :: Menu k a -> Bool
hasDone (p :|: q) = hasDone p || hasDone q
hasDone (p :/: q) = hasDone p || hasDone q
hasDone (p :|> _) = hasDone p
hasDone (_ :-> _) = False
hasDone Done      = True
hasDone Empty     = False

-- | Equality with a comparison function for the elements
eqMenuBy :: (k -> k -> Bool) -> (a -> a -> Bool) -> Menu k a -> Menu k a -> Bool
eqMenuBy eqK eqA = test
 where
   test (p1 :|: p2) (q1 :|: q2) = test p1 q1 && test p2 q2
   test (p1 :/: p2) (q1 :/: q2) = test p1 q1 && test p2 q2
   test (p1 :|> p2) (q1 :|> q2) = test p1 q1 && test p2 q2
   test (k1 :-> a1) (k2 :-> a2) = eqK k1 k2 && eqA a1 a2
   test Done        Done        = True
   test Empty       Empty       = True
   test (p :/: Empty) q = test p q
   test (p :|> Empty) q = test p q
   test p (q :/: Empty) = test p q
   test p (q :|> Empty) = test p q
   test _ _ = False

------------------------------------------------------------------------
-- Queries

-- | Returns all elements that are in the menu.
elems :: Menu k a -> [(k, a)]
elems = ($ []) . rec
 where
   rec (p :|: q) = rec p . rec q
   rec (p :/: q) = rec p . rec q
   rec (p :|> q) = rec p . rec q
   rec (k :-> a) = ((k, a):)
   rec Done      = id
   rec Empty     = id

-- | Returns only the best elements that are in the menu with respect to
-- left-biased choices.
bests :: Menu k a -> [(k, a)]
bests = bestsWith (++)

-- | Returns only the best elements that are in the menu, with a given ordering.
bestsOrdered :: (k -> k -> Ordering) -> Menu k a -> [(k, a)]
bestsOrdered cmp = bestsWith merge
 where
   -- merge two lists with comparison function
   merge lx@(x:xs) ly@(y:ys) =
      case cmp (fst x) (fst y) of
         GT -> y : merge lx ys
         _  -> x : merge xs ly
   merge [] ys = ys
   merge xs [] = xs

-- helper: takes combinator for (:|:)
bestsWith:: ([(k, a)] -> [(k, a)] -> [(k, a)]) -> Menu k a -> [(k, a)]
bestsWith f = rec
 where
   rec (p :|: q) = f (rec p) (rec q)
   rec (p :/: q) = rec p ++ rec q
   rec (p :|> _) = rec p
   rec (k :-> a) = [(k, a)]
   rec Done      = []
   rec Empty     = []

-- | Is the menu empty?
isEmpty :: Menu k a -> Bool
isEmpty Empty = True
isEmpty _     = False -- because of invariant

-- | Get an element from the menu by its index.
getByIndex :: Int -> Menu k a -> Maybe (k, a)
getByIndex n = listToMaybe . drop n . elems

-- | Only keep the best elements in the menu.
cut :: Menu k a -> Menu k a
cut (p :|: q) = cut p .|. cut q
cut (p :/: q) = cut p ./. cut q
cut (p :|> _) = cut p
cut (k :-> a) = k |-> a
cut Done      = doneMenu
cut Empty     = empty

------------------------------------------------------------------------
-- Generalized functions

-- | Generalized monadic bind, with the arguments flipped.
{-# INLINE onMenu #-}
onMenu :: Choice b => (k -> a -> b) -> b -> Menu k a -> b
onMenu f e = rec
 where
   rec (p :|: q) = rec p .|. rec q
   rec (p :/: q) = rec p ./. rec q
   rec (p :|> q) = rec p  |> rec q
   rec (k :-> a) = f k a
   rec Done      = e
   rec Empty     = empty

-- | Maps a function over a menu that also takes the index of an element.
{-# INLINE onMenuWithIndex #-}
onMenuWithIndex :: Choice b => (Int -> k -> a -> b) -> b -> Menu k a -> b
onMenuWithIndex f e = snd . rec 0
 where
   rec n (p :|: q) = let (n1, pn) = rec n p
                         (n2, qn) = rec n1 q
                     in (n2, pn .|. qn)
   rec n (p :/: q) = let (n1, pn) = rec n p
                         (n2, qn) = rec n1 q
                     in (n2, pn ./. qn)
   rec n (p :|> q) = let (n1, pn) = rec n p
                         (n2, qn) = rec n1 q
                     in (n2, pn |> qn)
   rec n (k :-> a) = (n+1, f n k a)
   rec n Done      = (n, e)
   rec n Empty     = (n, empty)