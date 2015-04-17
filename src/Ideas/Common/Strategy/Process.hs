{-# LANGUAGE TypeFamilies #-}
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
-- Processes must support choices and sequences. This module defines a type
-- class, an implementation, and utility functions.
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Process
   ( -- * IsProcess type class
     IsProcess(..), fromProcess
     -- * Process data type
   , Process, menu, eqProcessBy
     -- * Building sequences
   , Builder
     -- * Query functions on a Process
   , ready, stopped, firsts
     -- * Higher-order functions for iterating over a Process
   , fold, accum, scan, prune
   ) where

import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence

------------------------------------------------------------------------
-- IsProcess type class

class (Choice f, Sequence f) => IsProcess f where
   -- | Convert to the 'Process' data type.
   toProcess :: f a -> Process a

fromProcess :: IsProcess f => Process a -> f a
fromProcess = fold (~>) done

------------------------------------------------------------------------
-- Process data type

-- | This datatype implements choices and sequences, but is slow for
-- building sequences with the '<*>' combinator. See the 'Builder' data
-- type for a faster alternative.
newtype Process a = P (Menu (MenuItem a (Process a)))

instance Eq a => Eq (Process a) where
   (==) = eqProcessBy (==)

instance Functor Process where
   fmap f (P m) = P (fmap g m)
    where
      g Done = Done
      g (a :~> p) = f a :~> fmap f p

instance Choice Process where
   single a = P (single (a :~> P (single Done)))
   empty    = P empty
   P x <|> P y = P (x <|> y)
   P x >|> P y = P (x >|> y)
   P x  |> P y = P (x |> y)

instance Sequence Process where
   done   = P (return Done)
   a ~> p = P (return (a :~> p))

   p0 <*> P rest = rec p0
    where
      rec (P m) = P $ do
         st <- m -- cutOn (menuItem True (\_ _ -> False)) m
         case st of
            a :~> p -> return (a :~> rec p)
            Done    -> rest

instance IsProcess Process where
   toProcess = id

instance Firsts (Process a) where
   type Elem (Process a) = a

   menu (P m) = m

-- | Generalized equality of processes, which takes an equality function for
-- the symbols.
eqProcessBy :: (a -> a -> Bool) -> Process a -> Process a -> Bool
eqProcessBy eq = rec
 where
   rec p q = eqMenuBy eqStep (menu p) (menu q)

   eqStep (a :~> p) (b :~> q) = eq a b && rec p q
   eqStep Done      Done      = True
   eqStep _         _         = False

------------------------------------------------------------------------
-- Building sequences

-- | The 'Builder' data type offers a fast implementation for building
-- sequences. The data type implements the 'IsProcess' type class.
-- A 'Builder' value must be converted to a 'Process' (with 'toProcess')
-- it can be inspected in any way.

newtype Builder a = B (Process a -> Process a)

instance Choice Builder where
   single a = B (a ~>)
   empty    = B (const empty)
   B f <|> B g = B (\p -> f p <|> g p)
   B f >|> B g = B (\p -> f p >|> g p)
   B f  |> B g = B (\p -> f p  |> g p)

instance Sequence Builder where
   done        = B id
   a ~> B f    = B ((a ~>) . f)
   B f <*> B g = B (f . g)

instance IsProcess Builder where
   toProcess (B f) = f done

------------------------------------------------------------------------
-- Higher-order functions for iterating over a Process

-- | Folding over a process takes a function for single steps and for 'done'.
{-# INLINE fold #-}
fold :: Choice f => (a -> f b -> f b) -> f b -> Process a -> f b
fold op e = rec
 where
   rec = onMenu (menuItem e (\a -> op a . rec)) . menu

{-# INLINE accum #-}
accum :: (a -> b -> [b]) -> b -> Process a -> Menu b
accum f = rec
 where
   rec b p = menu p >>= g
    where
      g Done      = single b
      g (a :~> q) = choice [ rec b2 q  | b2 <- f a b ]

{-# INLINE scan #-}
scan :: (s -> a -> [(s, b)]) -> s -> Process a -> Process b
scan op = rec
 where
   rec s =
      let f a q = choice [ b ~> rec s2 q | (s2, b) <- op s a ]
      in onMenu (menuItem done f) . menu

-- fail early
prune :: (a -> Bool) -> Process a -> Process a
prune f = fold op done
 where
   op a p
      | not (f a) && stopped np = empty
      | otherwise               = a ~> np
    where
      np = P (cut (menu p))