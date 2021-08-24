{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-----------------------------------------------------------------------------

module Ideas.Utils.Decoding
   ( Decoder, evalDecoder, runDecoder, mapError
   , Encoder, runEncoder
     -- re-exports
   , Alternative(..), MonadReader(..), MonadState(..), MonadError(..)
   , gets
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-------------------------------------------------------------------

newtype Decoder env err s a = Dec { fromDec :: StateT s (ReaderT env (Except err)) a }
 deriving (Functor, Applicative, Alternative, MonadPlus, MonadReader env, MonadState s, MonadError err)

instance Semigroup a => Semigroup (Decoder env err s a) where
   (<>) = liftA2 (<>)

instance Monoid a => Monoid (Decoder env err s a) where
   mempty  = pure mempty
   mappend = liftA2 mappend

instance Monad (Decoder env err s) where
   return a    = Dec (return a)
   Dec m >>= f = Dec $ m >>= fromDec . f

runDecoder :: Decoder env err s a -> env -> s -> Either err (a, s)
runDecoder p env s = runExcept (runReaderT (runStateT (fromDec p) s) env)

evalDecoder :: Decoder env err s a -> env -> s -> Either err a
evalDecoder p env = fmap fst . runDecoder p env

mapError ::  (err1 -> err2) -> Decoder env err1 s a -> Decoder env err2 s a
mapError f p = do
   env <- reader id
   s1  <- get
   case runDecoder p env s1 of
      Left e1 -> throwError (f e1)
      Right (a, s2) -> put s2 >> return a

-------------------------------------------------------------------

type Encoder env err = Decoder env err ()

runEncoder :: Encoder env err a -> env -> Either err a
runEncoder p env = evalDecoder p env ()