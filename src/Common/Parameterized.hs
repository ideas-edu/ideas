{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Parameterization
--
-----------------------------------------------------------------------------
module Common.Parameterized 
   ( Parameterized
   , parameter1, parameter2, parameter3, unParam
   ) where

import Common.Binding
import Common.Classes
import Common.Id
import Common.View
import Data.Typeable

data Parameterized b a where
   Unit :: Environment -> a -> Parameterized () a
   Abs  :: Typeable b => Binding b -> (b -> Parameterized c a) -> Parameterized (b, c) a
   App  :: Parameterized (c, b) a -> c -> Parameterized b a
   Iso  :: Isomorphism c b -> Parameterized c a -> Parameterized b a

instance Functor (Parameterized b) where
   fmap f (Unit env a) = Unit env (f a)
   fmap f (Abs b g)    = Abs b (fmap f . g)
   fmap f (App a b)    = App (fmap f a) b
   fmap f (Iso v a)    = Iso v (fmap f a)

parameter1 :: Typeable b => Binding b -> (b -> a) -> Parameterized b a
parameter1 n f = Iso v $ Abs n (Unit mempty . f)
 where v = fst <-> (\a -> (a, ()))

parameter2 :: (Typeable b, Typeable c) => Binding b -> Binding c -> (b -> c -> a) -> Parameterized (b, c) a
parameter2 n1 n2 f = Iso v $ Abs n1 $ \b -> Abs n2 $ Unit mempty . f b
 where v = mapSecond fst <-> (\(a, b) -> (a, (b, ())))

parameter3 :: (Typeable b, Typeable c, Typeable d) => Binding b -> Binding c -> Binding d -> (b -> c -> d -> a) -> Parameterized (b, c, d) a
parameter3 n1 n2 n3 f = Iso v $ Abs n1 $ \b -> Abs n2 $ \c -> Abs n3 $ Unit mempty . f b c
 where v = (\(a, (b, (c, ()))) -> (a, b, c)) <-> (\(a, b, c) -> (a, (b, (c, ()))))

insert :: Typed Binding -> Parameterized b a -> Parameterized b a
insert b (Unit env a) = Unit (insertTypedBinding b env) a
insert b (Abs a f)    = Abs a (insert b . f)
insert b (App a c)    = App (insert b a) c
insert b (Iso v a)    = Iso v (insert b a)

unParam :: Parameterized b a -> b -> (a, Environment)
unParam (Unit env a) () = (a, env)
unParam (Abs a f) (b, c) = unParam (insert (Typed (setValue b a)) (f b)) c
unParam (App a c) b = unParam a (c, b)
unParam (Iso v a) b = unParam a (to v b)