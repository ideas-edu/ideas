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
-- Processes support choices and sequences and are modelled after Hoare's CSP
-- calculus.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Process
   ( Process, eqProcessBy, menu, withMenu
   , fold, runProcess
   ) where

import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence

------------------------------------------------------------------------
-- Process data type

-- | Process data type with efficient support for sequences
newtype Process a = P [Menu a (Process a)]

instance Eq a => Eq (Process a) where
   (==) = eqProcessBy (==)

instance Functor Process where
   fmap f = rec
    where
      rec (P xs) = P (map g xs)
      g = onMenu (\a q -> f a |-> rec q) doneMenu

instance Choice (Process a) where
   empty   = P [empty]
   x .|. y = P [menu x .|. menu y]
   x ./. y = P [menu x ./. menu y]
   x |>  y = P [menu x |>  menu y]

instance Sequence (Process a) where
   type Sym (Process a) = a

   done = P []
   a ~> b = P [a |-> b]
   P xs .*. P ys = P (xs ++ ys)
   sequence ps = P [ x | P xs <- ps, x <- xs ]

instance Fix (Process a)

instance Firsts (Process a) where
   type Elem (Process a) = a

   firsts = bests . menu
   ready  = hasDone . menu

runProcess :: Apply f => Process (f a) -> a -> [a]
runProcess p a = withMenu op [a] p
 where
   op f x = [ c | b <- applyAll f a, c <- runProcess x b ]

menu :: Process a -> Menu a (Process a)
menu (P zs) = rec zs
 where
   rec []     = doneMenu
   rec [x]    = x
   rec (x:xs) = onMenu (\a (P ys) -> a |-> P (ys ++ xs)) (rec xs) x

withMenu :: Choice b => (a -> Process a -> b) -> b -> Process a -> b
withMenu op e (P zs) = rec zs
 where
   rec []     = e
   rec [x]    = onMenu op e x
   rec (x:xs) = onMenu (\a (P ys) -> op a (P (ys ++ xs))) (rec xs) x

-- | Generalized equality of processes, which takes an equality function for
-- the symbols.
eqProcessBy :: (a -> a -> Bool) -> Process a -> Process a -> Bool
eqProcessBy eq = rec
 where
   rec p q = eqMenuBy eq rec (menu p) (menu q)

fold :: Choice b => (a -> b -> b) -> b -> Process a -> b
fold op e = rec
 where
   rec = withMenu (\a -> op a . rec) e