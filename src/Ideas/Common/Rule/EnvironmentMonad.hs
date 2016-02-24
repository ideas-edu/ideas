{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- State monad for environments
--
-----------------------------------------------------------------------------

module Ideas.Common.Rule.EnvironmentMonad
   ( -- * Environment Monad
     EnvMonad((:=), (:~), (:?))
   , getRef, updateRefs
     -- * Running the monad
   , runEnvMonad, execEnvMonad, evalEnvMonad
     -- * Extracting used references
   , envMonadRefs, envMonadFunctionRefs
   ) where

import Control.Applicative (Alternative(..))
import Control.Monad
import Data.Maybe
import Data.Typeable
import Ideas.Common.Environment
import Ideas.Common.Utils
import System.IO.Unsafe
import qualified Control.Exception as C

-----------------------------------------------------------
-- Environment Monad

infix 2 :=, :~, :?

data EnvMonad a where
   -- Monad operations
   Return :: a -> EnvMonad a
   Bind   :: EnvMonad a -> (a -> EnvMonad b) -> EnvMonad b
   Then   :: EnvMonad a -> EnvMonad b -> EnvMonad b
   Fail   :: String -> EnvMonad b
   -- MonadPlus operations
   Zero   :: EnvMonad a
   Plus   :: EnvMonad a -> EnvMonad a -> EnvMonad a
   -- References (special)
   (:=)   :: Typeable a => Ref a -> a -> EnvMonad ()
   (:~)   :: Typeable a => Ref a -> (a -> a) -> EnvMonad ()
   (:?)   :: Typeable a => Ref a -> a -> EnvMonad a
   GetRef :: Typeable a => Ref a -> EnvMonad a

instance Functor EnvMonad where
   fmap = liftM

instance Applicative EnvMonad where
   pure  = return
   (<*>) = ap

instance Alternative EnvMonad where
   empty = Zero
   (<|>) = Plus

instance Monad EnvMonad where
   return = Return
   (>>=)  = Bind
   fail   = Fail

instance MonadPlus EnvMonad where
   mzero = Zero
   mplus = Plus

getRef :: Typeable a => Ref a -> EnvMonad a
getRef = GetRef

updateRefs :: MonadPlus m => [EnvMonad a] -> Environment -> m Environment
updateRefs xs = msum . map return . execEnvMonad (sequence_ xs)

-----------------------------------------------------------
-- Environment Monad

runEnvMonad :: EnvMonad a -> Environment -> [(a, Environment)]
runEnvMonad envMonad env =
   case envMonad of
      Return a   -> [(a, env)]
      Bind m f   -> concat [ runEnvMonad (f a) e | (a, e) <- runEnvMonad m env ]
      Then m n   -> concat [ runEnvMonad n e     | (_, e) <- runEnvMonad m env ]
      Fail _     -> []
      Zero       -> []
      Plus m n   -> runEnvMonad m env ++ runEnvMonad n env
      ref := a   -> [((), insertRef ref a env)]
      ref :~ f   -> [((), changeRef ref f env)]
      ref :? a   -> [(fromMaybe a (ref ? env), env)]
      GetRef ref -> case ref ? env of
                       Just a  -> [(a, env)]
                       Nothing -> []

execEnvMonad :: EnvMonad a -> Environment -> [Environment]
execEnvMonad m = liftM snd . runEnvMonad m

evalEnvMonad :: EnvMonad a -> Environment -> [a]
evalEnvMonad m = liftM fst . runEnvMonad m

-----------------------------------------------------------
-- Extracting used references

envMonadRefs :: EnvMonad a -> [Some Ref]
envMonadRefs = unsafePerformIO . safeIO . envMonadRefsIO

envMonadFunctionRefs :: (a -> EnvMonad b) -> [Some Ref]
envMonadFunctionRefs = unsafePerformIO . safeIO . envMonadFunctionRefsIO

envMonadRefsIO :: EnvMonad a -> IO [Some Ref]
envMonadRefsIO monad =
   case monad of
      Bind m f -> envMonadRefsIO m ++++ envMonadFunctionRefsIO f
      Then a b -> envMonadRefsIO a ++++ envMonadRefsIO b
      Plus a b -> envMonadRefsIO a ++++ envMonadRefsIO b
      r := _   -> return [Some r]
      r :~ _   -> return [Some r]
      r :? _   -> return [Some r]
      _        -> return []
 where
   a ++++ b = liftM2 (++) (safeIO a) (safeIO b)

envMonadFunctionRefsIO :: (a -> EnvMonad b) -> IO [Some Ref]
envMonadFunctionRefsIO = safeIO . envMonadRefsIO . ($ error "catch me")

safeIO :: IO [a] -> IO [a]
safeIO m = m `C.catch` \(C.SomeException _) -> return []
