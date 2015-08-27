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
-- A type class for sequences together with the 'Step' datatype.
--
-----------------------------------------------------------------------------
--  $Id: Sequential.hs 6612 2014-06-12 07:57:59Z bastiaan $

module Ideas.Common.Strategy.Sequence
   ( -- * Sequence type class
     Sequence(..)
     -- * Firsts type class
   , Firsts(..), firstsOrdered, firstsTree, stopped
     -- * MenuItem data type with some utility functions
   ) where

import Ideas.Common.DerivationTree
import Ideas.Common.Strategy.Choice

infixr 5 <*>, ~>

------------------------------------------------------------------------
-- Sequence type class

class Sequence a where
   type Sym a
   -- | The empty sequence.
   done :: a
   -- | Prepend a symbol to a sequence.
   (~>) :: Sym a -> a -> a
   -- | Append two sequences.
   (<*>) :: a -> a -> a
   -- | Singleton sequence.
   single :: Sym a -> a
   single s = s ~> done
   -- | Sequential composition.
   sequence :: [a] -> a
   sequence xs = if null xs then done else foldr1 (<*>) xs
 
------------------------------------------------------------------------
-- Firsts type class

class Firsts s where
   -- | The type associated with a step in the first set.
   type Elem s
   -- | The ready predicate (we are done).
   ready :: s -> Bool
   ready = hasDone . menu
   -- | The first set.
   firsts :: s -> [(Elem s, s)]
   firsts = bests . menu
   -- | The menu offers single steps (with the remainder) and 'done' steps.
   menu :: s -> Menu (Elem s) s

firstsOrdered :: Firsts s => (Elem s -> Elem s -> Ordering)
              -> s -> [(Elem s, s)]
firstsOrdered cmp = bestsOrdered cmp . menu

firstsTree :: Firsts s => s -> DerivationTree (Elem s) s
firstsTree x = addBranches bs tr
 where
   tr = singleNode x (ready x)
   bs = [ (a, firstsTree y) | (a, y) <- firsts x ]

-- | Not ready and no further steps to take.
stopped :: Firsts s => s -> Bool
stopped = isEmpty . menu