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
   ( -- * Process data type
     Process, menu, eqProcessBy
     -- * Building sequences
   , Builder, mapBuilder, toBuilder, fromBuilder
     -- * Query functions on a Process
   , ready, firsts
   , runProcess
     -- * Higher-order functions for iterating over a Process
   , fold
   ) where

import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence

------------------------------------------------------------------------
-- Process data type

-- | This datatype implements choices and sequences, but is slow for
-- building sequences with the '<*>' combinator. See the 'Builder' data
-- type for a faster alternative.
newtype Process a = P { menu :: Menu a (Process a) }

instance Eq a => Eq (Process a) where
   (==) = eqProcessBy (==)

instance Choice (Process a) where
   empty   = P empty
   x <|> y = P (menu x <|> menu y)
   x >|> y = P (menu x >|> menu y)
   x  |> y = P (menu x  |> menu y)

instance Sequence (Process a) where
   type Sym (Process a) = a

   done   = P doneMenu
   a ~> p = P (a |-> p)
   
   p0 <*> P rest = rec p0
    where
      rec   = P . onMenu f rest . menu
      f a p = a |-> rec p

instance Firsts (Process a) where
   type Elem (Process a) = a

   ready  = hasDone . menu
   firsts = bests . menu

-- | Generalized equality of processes, which takes an equality function for
-- the symbols.
eqProcessBy :: (a -> a -> Bool) -> Process a -> Process a -> Bool
eqProcessBy eq = rec
 where
   rec p q = eqMenuBy eq rec (menu p) (menu q)
   
runProcess :: Apply f => Process (f a) -> a -> [a]
runProcess p a = onMenu op [a] (menu p)
 where
   op f x = [ c | b <- applyAll f a, c <- runProcess x b ]
   
mapProcess :: (a -> a) -> Process a -> Process a
mapProcess f = rec
 where
   rec   = onMenu g done . menu
   g a p = P (f a |-> rec p)
   
------------------------------------------------------------------------
-- Building sequences

-- | The 'Builder' data type offers a fast implementation for building
-- sequences. The data type implements the 'IsProcess' type class.
-- A 'Builder' value must be converted to a 'Process' (with 'toProcess')
-- it can be inspected in any way.

newtype Builder a = B (Process a -> Process a)

instance Choice (Builder a) where
   empty       = B empty
   B f <|> B g = B (f <|> g)
   B f >|> B g = B (f >|> g)
   B f  |> B g = B (f  |> g)

instance Sequence (Builder a) where
   type Sym (Builder a) = a

   done        = B id
   a ~> B f    = B ((a ~>) . f)
   B f <*> B g = B (f . g)

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
   f a = op a . rec
    
{-  
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
      np = cutProcess p 
        
cutProcess :: Process a -> Process a
cutProcess = P . cut . menu -}