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
-- State monad for environments
--
-----------------------------------------------------------------------------
module Common.Rule.EnvironmentMonad
   ( -- * Environment Monad
     EnvMonad((:=), (:~), (:?))
   , getRef, updateRefs
   , runEnvMonad, execEnvMonad, evalEnvMonad
   , envMonadRefs, envMonadFunctionRefs
   ) where

import Common.Environment
import Common.Utils
import Data.Maybe
import Data.Typeable
import Control.Monad.State
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

runEnvMonad :: EnvMonad a -> Environment -> [(a, Environment)]
runEnvMonad = runStateT . rec
 where
   rec :: EnvMonad a -> StateT Environment [] a
   rec monad =
      case monad of
         Return a   -> return a
         Bind m f   -> rec m >>= rec . f
         Then m n   -> rec m >> rec n
         Fail s     -> fail s
         Zero       -> mzero
         Plus m n   -> rec m `mplus` rec n
         ref := a   -> modify (insertRef ref a)
         ref :~ f   -> modify (changeRef ref f)
         ref :? a   -> gets (fromMaybe a . (ref ?))
         GetRef ref -> gets (ref ?) >>= maybe (fail "getRef") return

execEnvMonad :: EnvMonad a -> Environment -> [Environment]
execEnvMonad m = liftM snd . runEnvMonad m

evalEnvMonad :: EnvMonad a -> Environment -> [a]
evalEnvMonad m = liftM fst . runEnvMonad m

envMonadRefs :: EnvMonad a -> IO [Some Ref]
envMonadRefs monad =
   case monad of
      Bind m f -> envMonadRefs m ++++ envMonadFunctionRefs f
      Then a b -> envMonadRefs a ++++ envMonadRefs b
      Plus a b -> envMonadRefs a ++++ envMonadRefs b
      r := _   -> return [Some r]
      r :~ _   -> return [Some r]
      r :? _   -> return [Some r]
      _        -> return []
 where
   (++++) = liftM2 (++)

envMonadFunctionRefs :: (a -> EnvMonad b) -> IO [Some Ref]
envMonadFunctionRefs f = envMonadRefs (f (error "catch me")) 
   `C.catch` \(C.ErrorCall _) -> return []