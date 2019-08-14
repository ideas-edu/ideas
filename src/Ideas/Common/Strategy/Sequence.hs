{-# LANGUAGE TypeFamilies #-}
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
-- A type class for sequences together with the 'Firsts' type class for
-- accessing the firsts set and ready predicate.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Sequence
   ( -- * Sequence type class
     Sequence(..)
     -- * Firsts type class
   , Firsts(..), firstsTree
   ) where

import Ideas.Common.DerivationTree

infixr 5 .*., ~>

------------------------------------------------------------------------
-- Sequence type class

class Sequence a where
   type Sym a
   -- | The empty sequence.
   done :: a
   -- | Prepend a symbol to a sequence.
   (~>) :: Sym a -> a -> a
   -- | Append two sequences.
   (.*.) :: a -> a -> a
   -- | Singleton sequence.
   single :: Sym a -> a
   single s = s ~> done
   -- | Sequential composition.
   sequence :: [a] -> a
   sequence xs = if null xs then done else foldr1 (.*.) xs

instance Sequence b => Sequence (a -> b) where
   type Sym (a -> b) = Sym b

   done   = const done
   single = const . single
   a ~> f = (a ~>) . f
   (f .*. g) x = f x .*. g x

------------------------------------------------------------------------
-- Firsts type class

class Firsts s where
   -- | The type associated with a step in the first set.
   type Elem s
   -- | The ready predicate (we are done).
   ready :: s -> Bool
   -- | The firsts set.
   firsts :: s -> [(Elem s, s)]

firstsTree :: Firsts s => s -> DerivationTree (Elem s) s
firstsTree x = addBranches bs tr
 where
   tr = singleNode x (ready x)
   bs = [ (a, firstsTree y) | (a, y) <- firsts x ]