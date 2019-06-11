{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ideas.Utils.Decoding 
   ( Decoder, runDecoder, symbol
   , Encoder, runEncoder
   , Error, runError, runErrorM
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Semigroup as Sem

-------------------------------------------------------------------

newtype Decoder env s a = Dec { runDec :: StateT s (ReaderT env Error) a }
 deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadReader env, MonadState s)

instance Sem.Semigroup a => Sem.Semigroup (Decoder env s a) where
   (<>) = liftA2 (<>)

instance Monoid a => Monoid (Decoder env s a) where
   mempty  = pure mempty
   mappend = liftA2 mappend

symbol :: Decoder env [s] s
symbol = get >>= \list ->
   case list of
      []   -> fail "Empty input"
      x:xs ->
         put xs >> return x

runDecoder :: Monad m => Decoder env s a -> env -> s -> m a
runDecoder p env s = runErrorM (runReaderT (evalStateT (runDec p) s) env)

-------------------------------------------------------------------

type Encoder env = Decoder env ()

runEncoder :: Monad m => Encoder env a -> env -> m a
runEncoder p env = runDecoder p env ()

-------------------------------------------------------------------
-- Error monad (helper)

newtype Error a = Error { runError :: Either String a }

instance Functor Error where
   fmap f = Error . fmap f . runError

instance Applicative Error where
   pure    = Error . Right
   p <*> q = Error $
      case (runError p, runError q) of
         (Left s, _)  -> Left s
         (_, Left s)  -> Left s
         (Right f, Right x) -> Right (f x)

instance Alternative Error where
   empty   = Error (Left "empty")
   p <|> q = Error $
      case (runError p, runError q) of
         (Right a, _) -> Right a
         (_, Right a) -> Right a
         (Left s, _)  -> Left s

instance Monad Error where
   fail    = Error . Left
   return  = pure
   m >>= f = Error $ either Left (runError . f) (runError m)

instance MonadPlus Error where
   mzero = fail "mzero"
   mplus = (<|>)

runErrorM :: Monad m => Error a -> m a
runErrorM = either fail return . runError