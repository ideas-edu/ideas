{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
-- Typeable type class, with the IsTypeable data type for witnessing instances
--
-----------------------------------------------------------------------------

module Ideas.Utils.Typeable
   ( IsTypeable, typeable
   , HasTypeable(..)
   , castFrom, castTo, castBetween
   , gcastFrom, gcastTo, gcastBetween
   , module Data.Typeable
   ) where

import Control.Monad
import Data.Typeable
import Unsafe.Coerce

newtype IsTypeable a = IT TypeRep

class HasTypeable f where
   getTypeable :: f a -> Maybe (IsTypeable a)

instance HasTypeable IsTypeable where
   getTypeable = Just

typeable :: forall a . Typeable a => IsTypeable a
typeable = IT (typeRep (Proxy :: Proxy a))

castFrom :: (HasTypeable f, Typeable b) => f a -> a -> Maybe b
castFrom = flip castBetween typeable

castTo :: (HasTypeable f, Typeable a) => f b -> a -> Maybe b
castTo = castBetween typeable

castBetween :: (HasTypeable f, HasTypeable g) => f a -> g b -> a -> Maybe b
castBetween x y a = do
   guardEq x y
   return $ unsafeCoerce a

gcastFrom :: (HasTypeable f, Typeable b) => f a -> c a -> Maybe (c b)
gcastFrom = flip gcastBetween typeable

gcastTo :: (HasTypeable f, Typeable a) => f b -> c a -> Maybe (c b)
gcastTo = gcastBetween typeable

gcastBetween :: (HasTypeable f, HasTypeable g) => f a -> g b -> c a -> Maybe (c b)
gcastBetween ta tb x = fmap (\Refl -> x) (eqIT ta tb)

eqIT :: (HasTypeable f, HasTypeable g) => f a -> g b -> Maybe (a :~: b)
eqIT x y = do
   guardEq x y
   return $ unsafeCoerce Refl

guardEq :: (HasTypeable f, HasTypeable g) => f a -> g b -> Maybe ()
guardEq x y = do
   IT ta <- getTypeable x
   IT tb <- getTypeable y
   guard (ta == tb)