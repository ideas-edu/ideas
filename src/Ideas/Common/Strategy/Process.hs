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
     IsProcess(..)
     -- * Process data type
   , Process, menu, eqProcessBy
     -- * Building sequences
   , Builder, mapBuilder, toBuilder, fromBuilder
     -- * Query functions on a Process
   , ready, stopped, firsts
   , runProcess
     -- * Higher-order functions for iterating over a Process
   , fold, accum, scan, prune
   ) where

import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence

------------------------------------------------------------------------
-- IsProcess type class

infixr 5 ~>

class IsProcess f where
   -- | Convert to the 'Process' data type.
   single    :: a -> f a
   (~>)      :: a -> f a -> f a

------------------------------------------------------------------------
-- Process data type

-- | This datatype implements choices and sequences, but is slow for
-- building sequences with the '<*>' combinator. See the 'Builder' data
-- type for a faster alternative.
newtype Process a = P (Menu a (Process a))

instance Eq a => Eq (Process a) where
   (==) = eqProcessBy (==)

instance Choice (Process a) where
   empty    = P empty
   P x <|> P y = P (x <|> y)
   P x >|> P y = P (x >|> y)
   P x  |> P y = P (x |> y)

instance Sequence (Process a) where
   done   = P doneMenu

   p0 <*> P rest = rec p0
    where
      rec (P m) = P (onMenu f rest m)
      
      f a p = a |-> rec p

instance IsProcess Process where
   single a  = P (a |-> done)
   a ~> p    = P (a |-> p)

instance Firsts (Process a) where
   type Elem (Process a) = a

   menu (P m) = m

-- | Generalized equality of processes, which takes an equality function for
-- the symbols.
eqProcessBy :: (a -> a -> Bool) -> Process a -> Process a -> Bool
eqProcessBy eq = rec
 where
   rec p q = eqMenuBy eq rec (menu p) (menu q)

   --eqStep (Just a) (Just b) = eq a b
   --eqStep Nothing  Nothing  = True
   --eqStep _        _        = False

runProcess :: Apply f => Process (f a) -> a -> [a]
runProcess p a = map fst $ bests (accum applyFst a () p) 
 where
   applyFst f x y = [ (z, y) | z <- applyAll f x ]
   
mapProcess :: (a -> a) -> Process a -> Process a
mapProcess f = rec
 where
   rec (P m) = onMenu g done m
   g a p     = P (f a |-> rec p)
   
------------------------------------------------------------------------
-- Building sequences

-- | The 'Builder' data type offers a fast implementation for building
-- sequences. The data type implements the 'IsProcess' type class.
-- A 'Builder' value must be converted to a 'Process' (with 'toProcess')
-- it can be inspected in any way.

newtype Builder a = B (Process a -> Process a)

instance Choice (Builder a) where
   empty    = B (const empty)
   B f <|> B g = B (\p -> f p <|> g p)
   B f >|> B g = B (\p -> f p >|> g p)
   B f  |> B g = B (\p -> f p  |> g p)

instance Sequence (Builder a) where
   done        = B id
   B f <*> B g = B (f . g)

instance IsProcess Builder where
   single a        = B (a ~>)
   a ~> B f        = B ((a ~>) . f)

mapBuilder :: (a -> a) -> Builder a -> Builder a
mapBuilder f (B g) = B (mapProcess f . g)

toBuilder :: Process a -> Builder a
toBuilder p = B (p <*>)

fromBuilder :: Builder a -> Process a
fromBuilder (B f) = f done

------------------------------------------------------------------------
-- Higher-order functions for iterating over a Process

-- | Folding over a process takes a function for single steps and for 'done'.
{-# INLINE fold #-}
fold :: Choice b => (a -> b -> b) -> b -> Process a -> b
fold op e = rec
 where
   rec = onMenu f e . menu
   f a p = op a (rec p)
   
{-# INLINE accum #-}
accum :: (a -> k -> b -> [(k, b)]) -> k -> b -> Process a -> Menu k b
accum f = rec
 where
   rec k b = onMenu g (k |-> b) . menu
    where
      g a q = choice [ rec k2 b2 q  | (k2, b2) <- f a k b ]
      
{-# INLINE scan #-}
scan :: (s -> a -> [(s, b)]) -> s -> Process a -> Process b
scan op = rec
 where
   rec s =
      let f a q = choice [ b ~> rec s2 q | (s2, b) <- op s a ]
      in onMenu f done . menu

-- fail early
prune :: (a -> Bool) -> Process a -> Process a
prune f = fold op done
 where
   op a p
      | not (f a) && stopped np = empty
      | otherwise               = a ~> np
    where
      np = P (cut (menu p))