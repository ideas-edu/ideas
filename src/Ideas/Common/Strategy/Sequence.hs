{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A type class for sequences together with the 'Step' datatype. 
-- 
-----------------------------------------------------------------------------
--  $Id: Sequential.hs 6612 2014-06-12 07:57:59Z bastiaan $

module Ideas.Common.Strategy.Sequence 
   ( -- * Sequence type class
     Sequence(..)
     -- * Firsts type class
   , Firsts(..), firstsMenu, firstsOrdered, firstsTree, stopped
     -- * MenuItem data type with some utility functions
   , MenuItem(..), menuItem, isDone
   ) where

import Data.Function
import Ideas.Common.Classes
import Ideas.Common.DerivationTree
import Ideas.Common.Strategy.Choice

infixr 5 :~>, ~>, <*>

------------------------------------------------------------------------
-- Sequence type class

class Sequence f where
   -- | The empty sequence.
   done :: f a
   -- | Prefix a sequence with one element. 
   (~>) :: a -> f a -> f a
   -- | Append two sequences.
   (<*>) :: f a -> f a -> f a

------------------------------------------------------------------------
-- Firsts type class

class Firsts s where
   -- | The type associated with a step in the first set.
   type Elem s
   -- | The ready predicate (we are done).
   ready :: s -> Bool
   ready = any isDone . bests . menu
   -- | The first set.
   firsts :: s -> [(Elem s, s)]
   firsts = bests . firstsMenu
   -- | The menu offers single steps (with the remainder) and 'done' steps.
   menu :: s -> Menu (MenuItem (Elem s) s)
   
firstsMenu :: Firsts s => s -> Menu (Elem s, s)
firstsMenu s = do
   item <- menu s
   case item of
      a :~> t -> return (a, t)
      Done    -> empty

firstsOrdered :: Firsts s => (Elem s -> Elem s -> Ordering) 
              -> s -> [(Elem s, s)]
firstsOrdered cmp = bestsOrdered (cmp `on` fst) . firstsMenu
   
firstsTree :: Firsts s => s -> DerivationTree (Elem s) s
firstsTree x = addBranches bs tr
 where
   tr = singleNode x (ready x)
   bs = [ (a, firstsTree y) | (a, y) <- firsts x ]

-- | Not ready and no further steps to take.
stopped :: Firsts s => s -> Bool
stopped = isEmpty . menu

------------------------------------------------------------------------
-- MenuItem data type with some utility functions
 
data MenuItem a s = a :~> s   -- ^ A single step.
                  | Done      -- ^ No step (we are done).

instance Functor (MenuItem a) where
   fmap = mapSecond

instance BiFunctor MenuItem where
   biMap f g = menuItem Done (\a s -> f a :~> g s)

-- | The 'menuItem' function takes a default value for 'Done' and a function 
-- to combine the values for a single step.
menuItem :: b -> (a -> s -> b) -> MenuItem a s -> b
menuItem b _ Done      = b
menuItem _ f (a :~> x) = f a x

-- | Is the item 'done'?
isDone :: MenuItem a s -> Bool
isDone Done = True
isDone _    = False