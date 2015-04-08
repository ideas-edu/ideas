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
-- A type class with an implementation for expressing choice and left-biased
-- choice.
--
-----------------------------------------------------------------------------
--  $Id: Sequential.hs 6598 2014-06-04 14:59:01Z bastiaan $

module Ideas.Common.Strategy.Choice
   ( -- * Choice type class
     Choice(..)
     -- * Menu data type
   , Menu, eqMenuBy
     -- * Queries
   , elems, bests, bestsOrdered, isEmpty, getByIndex
     -- * Generalized functions
   , onMenu, cut, cutOn, mapWithIndex
   ) where

import Data.Maybe (listToMaybe)

infixr 3 <|>, >|>, |>, :|:, :>|, :|>

------------------------------------------------------------------------
-- Choice type class

-- | Laws: '<|>', '>|>' '|>' are all associative, and have 'empty' as their
-- unit element.
class Choice f where
   -- | Nothing to choose from.
   empty :: f a
   -- | One element.
   single :: a -> f a
   -- | Normal (unbiased) choice.
   (<|>) :: f a -> f a -> f a
   -- | Left-preference.
   (>|>) :: f a -> f a -> f a
   -- | Left-biased choice.
   (|>) :: f a -> f a -> f a
   -- | One element from a list (unbiased).
   oneof :: [a]   -> f a
   -- | One of the alternatives in a list (unbiased).
   choice :: [f a] -> f a
   -- default implementation
   oneof = choice . map single
   choice xs
      | null xs   = empty
      | otherwise = foldr1 (<|>) xs

instance Choice [] where
   empty    = []
   single   = return
   (<|>)    = (++)
   (>|>)    = (++)
   xs |> ys = if null xs then ys else xs
   oneof    = id
   choice   = concat

------------------------------------------------------------------------
-- Menu data type

-- Invariants for the Menu datatype:
-- (Unit) The left-hand side of :|: and :|> cannot be Empty
-- (Asso) :|: and :|> are balanced to the right

-- | A menu offers choices and preferences. It is an instance of the 'Functor'
-- and 'Monad' type classes.
data Menu a = Single a
            | Empty
            | Menu a :|: Menu a
            | Menu a :>| Menu a -- left-preference
            | Menu a :|> Menu a -- left-biased

instance Eq a => Eq (Menu a) where
   (==) = eqMenuBy (==)

instance Choice Menu where
   empty  = Empty
   single = Single

   p0 <|> rest = rec p0 -- maintain invariant
    where
     rec Empty     = rest
     rec (p :|: q) = p :|: rec q
     rec p         = case rest of -- strict: also check rhs
                        Empty -> p
                        _     -> p :|: rest

   p0 >|> rest = rec p0 -- maintain invariant
    where
     rec Empty     = rest
     rec (p :>| q) = p :>| rec q
     rec p         = p :>| rest

   p0 |> rest = rec p0 -- maintain invariant
    where
     rec Empty     = rest
     rec (p :|> q) = p :|> rec q
     rec p         = p :|> rest

instance Functor Menu where
   fmap f p = p >>= (Single . f)

instance Monad Menu where
   return = single
   fail _ = empty
   (>>=)  = flip onMenu

-- | Equality with a comparison function for the elements
eqMenuBy :: (a -> a -> Bool) -> Menu a -> Menu a -> Bool
eqMenuBy eq = test
 where
   test (p1 :|: p2) (q1 :|: q2) = test p1 q1 && test p2 q2
   test (p1 :>| p2) (q1 :>| q2) = test p1 q1 && test p2 q2
   test (p1 :|> p2) (q1 :|> q2) = test p1 q1 && test p2 q2
   test (Single a)  (Single b)  = eq a b
   test Empty       Empty       = True
   test (p :>| Empty) q = test p q
   test (p :|> Empty) q = test p q
   test p (q :>| Empty) = test p q
   test p (q :|> Empty) = test p q
   test _ _ = False

------------------------------------------------------------------------
-- Queries

-- | Returns all elements that are in the menu.
elems :: Menu a -> [a]
elems = ($ []) . rec
 where
   rec (p :|: q)  = rec p . rec q
   rec (p :>| q)  = rec p . rec q
   rec (p :|> q)  = rec p . rec q
   rec (Single p) = (p:)
   rec Empty      = id

-- | Returns only the best elements that are in the menu.
bests :: Menu a -> [a]
bests (p :|: q)  = bests p ++ bests q
bests (p :>| q)  = bests p ++ bests q
bests (p :|> q)  = bests p |> bests q
bests (Single a) = [a]
bests Empty      = []

-- | Returns only the best elements that are in the menu, with a given ordering.
bestsOrdered :: (a -> a -> Ordering) -> Menu a -> [a]
bestsOrdered cmp = rec
 where
   rec (p :|: q)  = merge (rec p) (rec q)
   rec (p :>| q)  = rec p ++ rec q
   rec (p :|> q)  = rec p |> rec q
   rec (Single a) = [a]
   rec Empty      = []

   -- merge two lists with comparison function
   merge lx@(x:xs) ly@(y:ys)
      | cmp x y == GT = y : merge lx ys
      | otherwise     = x : merge xs ly
   merge [] ys = ys
   merge xs [] = xs

-- | Is the menu empty?
isEmpty :: Menu a -> Bool
isEmpty Empty = True
isEmpty _     = False -- because of invariant

-- | Get an element from the menu by its index.
getByIndex :: Int -> Menu a -> Maybe a
getByIndex n = listToMaybe . drop n . elems

------------------------------------------------------------------------
-- Generalized functions

-- | Generalized monadic bind, with the arguments flipped.
{-# INLINE onMenu #-}
onMenu :: Choice f => (a -> f b) -> Menu a -> f b
onMenu f = rec
 where
   rec (p :|: q)  = rec p <|> rec q
   rec (p :>| q)  = rec p >|> rec q
   rec (p :|> q)  = rec p  |> rec q
   rec (Single a) = f a
   rec Empty      = empty

-- | Only keep the best elements in the menu.
{-# INLINE cut #-}
cut :: Choice f => Menu a -> f a
cut (p :|: q)  = cut p <|> cut q
cut (p :>| q)  = cut p >|> cut q
cut (p :|> _)  = cut p
cut (Single a) = single a
cut Empty      = empty

cutOn :: Choice f => (a -> Bool) -> Menu a -> f a
cutOn f = snd . rec
 where
   rec (p :|: q)  = let (b1, cp) = rec p
                        (b2, cq) = rec q
                    in (b1 || b2, cp <|> cq)
   rec (p :>| q)  = let (b1, cp) = rec p
                        (b2, cq) = rec q
                    in (b1 || b2, cp >|> cq)
   rec (p :|> q)  = let (b1, cp) = rec p
                        (b2, cq) = rec q
                    in (b1 || b2, if b1 then cp else cp |> cq)
   rec (Single a) = (f a, single a)
   rec Empty      = (False, empty)

-- | Maps a function over a menu that also takes the index of an element.
{-# INLINE mapWithIndex #-}
mapWithIndex :: Choice f => (Int -> a -> f b) -> Menu a -> f b
mapWithIndex f = snd . rec 0
 where
   rec n (p :|: q)  = let (n1, pn) = rec n p
                          (n2, qn) = rec n1 q
                      in (n2, pn <|> qn)
   rec n (p :>| q)  = let (n1, pn) = rec n p
                          (n2, qn) = rec n1 q
                      in (n2, pn >|> qn)
   rec n (p :|> q)  = let (n1, pn) = rec n p
                          (n2, qn) = rec n1 q
                      in (n2, pn |> qn)
   rec n (Single a) = (n+1, f n a)
   rec n Empty      = (n, empty)