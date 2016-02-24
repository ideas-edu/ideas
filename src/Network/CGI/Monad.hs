{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Monad
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  John Chee <cheecheeo@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal stuff that most people shouldn't have to use.
-- This module mostly deals with the
-- internals of the CGIT monad transformer.
--
-----------------------------------------------------------------------------

module Network.CGI.Monad (
  -- * CGI monad class
  MonadCGI(..),
  -- * CGI monad transformer
  CGIT(..), CGI,
  runCGIT,
  -- * Request info
  CGIRequest(..),
  -- * Error handling
  throwCGI, catchCGI, tryCGI, handleExceptionCGI,
 ) where

import Control.Exception as Exception (SomeException)
import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask, throwM, catch, try, mask, uninterruptibleMask)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Control.Monad.Writer (WriterT(..), tell)
-- #if MIN_VERSION_base(4,7,0)
-- import Data.Typeable
-- #else
-- import Data.Typeable (Typeable(..), Typeable1(..),
--                       mkTyConApp, mkTyCon)
-- #endif

import Network.CGI.Protocol

--
-- * CGIT monad transformer
--

-- | A simple CGI monad with just IO.
type CGI a = CGIT IO a

-- | The CGIT monad transformer.
newtype CGIT m a = CGIT { unCGIT :: ReaderT CGIRequest (WriterT Headers m) a }
-- #if MIN_VERSION_base(4,7,0)
-- 			deriving (Typeable)
--
-- #else
-- instance (Typeable1 m, Typeable a) => Typeable (CGIT m a) where
--     typeOf _ = mkTyConApp (mkTyCon "Network.CGI.Monad.CGIT")
--                 [typeOf1 (undefined :: m a), typeOf (undefined :: a)]
-- #endif

instance (Functor m, Monad m) => Functor (CGIT m) where
    fmap f c = CGIT (fmap f (unCGIT c))

instance (Applicative m, Monad m) => Applicative (CGIT m) where
    pure = CGIT . pure
    f <*> x = CGIT (unCGIT f <*> unCGIT x)

instance Monad m => Monad (CGIT m) where
    c >>= f = CGIT (unCGIT c >>= unCGIT . f)
    return = CGIT . return
    -- FIXME: should we have an error monad instead?
    fail = CGIT . fail

instance MonadIO m => MonadIO (CGIT m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (CGIT m) where
    throwM e = CGIT . throwM $ e

instance MonadCatch m => MonadCatch (CGIT m) where
    CGIT m `catch` h = CGIT $ m `catch` (unCGIT . h)

instance MonadMask m => MonadMask (CGIT m) where
    mask a = CGIT $ mask $ \u -> unCGIT $ a $ CGIT . u . unCGIT
    uninterruptibleMask a = CGIT $ uninterruptibleMask $ \u -> unCGIT $ a $ CGIT . u . unCGIT

-- | The class of CGI monads. Most CGI actions can be run in
--   any monad which is an instance of this class, which means that
--   you can use your own monad transformers to add extra functionality.
class Monad m => MonadCGI m where
    -- | Add a response header.
    cgiAddHeader :: HeaderName -> String -> m ()
    -- | Get something from the CGI request.
    cgiGet :: (CGIRequest -> a) -> m a

instance Monad m => MonadCGI (CGIT m) where
    cgiAddHeader n v = CGIT $ lift $ tell [(n,v)]
    cgiGet = CGIT . asks

instance MonadTrans CGIT where
    lift = CGIT . lift . lift

-- | Run a CGI action.
runCGIT :: Monad m => CGIT m a -> CGIRequest -> m (Headers, a)
runCGIT (CGIT c) = liftM (uncurry (flip (,))) . runWriterT . runReaderT c

--
-- * Error handling
--

instance MonadCatch m => MonadError SomeException (CGIT m) where
    throwError = throwM
    catchError = catch

-- | Throw an exception in a CGI monad. The monad is required to be
--   a 'MonadThrow', so that we can use 'throwM' to guarantee ordering.
throwCGI :: (MonadCGI m, MonadThrow m) => SomeException -> m a
throwCGI = throwM

-- | Catches any expection thrown by a CGI action, and uses the given
--   exception handler if an exception is thrown.
catchCGI :: (MonadCGI m, MonadCatch m) => m a -> (SomeException -> m a) -> m a
catchCGI = catch

-- | Catches any exception thrown by an CGI action, and returns either
--   the exception, or if no exception was raised, the result of the action.
tryCGI :: (Functor m, MonadCGI m, MonadCatch m) => m a -> m (Either SomeException a)
tryCGI = try

{-# DEPRECATED handleExceptionCGI "Use catchCGI instead." #-}
-- | Deprecated version of 'catchCGI'. Use 'catchCGI' instead.
handleExceptionCGI :: (MonadCGI m, MonadCatch m) => m a -> (SomeException -> m a) -> m a
handleExceptionCGI = catchCGI
