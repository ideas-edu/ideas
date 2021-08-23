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
   ( Decoder, runDecoder, symbol
   , Encoder, runEncoder
   , Error, runError
   , throwError
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.String

-------------------------------------------------------------------

newtype Decoder env err s a = Dec { runDec :: StateT s (ReaderT env (Error err)) a }
 deriving (Functor, Applicative, Alternative, MonadPlus, MonadReader env, MonadState s, MonadError err)

instance Semigroup a => Semigroup (Decoder env err s a) where
   (<>) = liftA2 (<>)

instance Monoid a => Monoid (Decoder env err s a) where
   mempty  = pure mempty
   mappend = liftA2 mappend

instance IsString err => Monad (Decoder env err s) where
   return a    = Dec (return a)
   Dec m >>= f = Dec $ m >>= \a -> runDec (f a)
   fail s      = Dec $ throwError (fromString s) -- ! needed for backwards compatibility

symbol :: IsString err => Decoder env err [s] s
symbol = get >>= \list ->
   case list of
      []   -> fail "Empty input"
      x:xs ->
         put xs >> return x

runDecoder :: Decoder env err s a -> env -> s -> Either err a
runDecoder p env s = runError (runReaderT (evalStateT (runDec p) s) env)

-------------------------------------------------------------------

type Encoder env err = Decoder env err ()

runEncoder :: Encoder env err a -> env -> Either err a
runEncoder p env = runDecoder p env ()

-------------------------------------------------------------------
-- Error monad (helper)

type Error err = Except err

runError :: Error err a -> Either err a
runError = runExcept 