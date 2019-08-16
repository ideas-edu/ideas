{-# LANGUAGE ExistentialQuantification #-}
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
-- Representation for predicates
--
-----------------------------------------------------------------------------

module Ideas.Common.Predicate
   ( -- * Predicate representation
     Predicate, predicate, predicateView
   , evalPredicate
     -- * Exports from Boolean algebra
   , BoolValue(..), Boolean(..)
   , ands, ors, implies, equivalent
   ) where

import Ideas.Common.Classes
import Ideas.Common.Id
import Ideas.Common.View

data Predicate a
   = Const Bool
   | Prim (a -> Bool)
   | forall b . PView (View a b)
   | Compl (Predicate a)
   | Predicate a :&&: Predicate a
   | Predicate a :||: Predicate a
   | Id :@ Predicate a

instance BoolValue (Predicate a) where
   fromBool = Const
   isTrue  (Const b) = b
   isTrue  _         = False
   isFalse (Const b) = not b
   isFalse _         = False

instance Boolean (Predicate a) where
   Const b <&&> y       = if b then y else false
   x       <&&> Const b = if b then x else false
   x       <&&> y       = x :&&: y
   Const b <||> y       = if b then true else y
   x       <||> Const b = if b then true else x
   x       <||> y       = x :||: y
   complement (Const b) = Const (not b)
   complement x         = Compl x

instance HasId (Predicate a) where
   getId (n :@ _)  = n
   getId (PView v) = getId v
   getId _         = mempty
   changeId f (n :@ a) = f n :@ a
   changeId f a        = f mempty :@ a

instance Identify (Predicate a) where
   n @> v | a == mempty = v
          | otherwise   = a :@ v
    where
      a = newId n

predicate :: (a -> Bool) -> Predicate a
predicate = Prim

predicateView :: View a b -> Predicate a
predicateView = PView

evalPredicate :: Predicate a -> a -> Bool
evalPredicate p a = rec p
 where
   rec (Const b)  = b
   rec (Prim f)   = f a
   rec (PView v)  = a `belongsTo` v
   rec (Compl x)  = not (rec x)
   rec (x :&&: y) = rec x && rec y
   rec (x :||: y) = rec x || rec y
   rec (_ :@ x)   = rec x