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
module Ideas.Common.Rule.EnvironmentMonad
   ( -- * Environment Monad
     EnvMonad((:=), (:~), (:?))
   , getRef, updateRefs
     -- * Running the monad
   , runEnvMonad, execEnvMonad, evalEnvMonad
     -- * Extracting used references
   , envMonadRefs, envMonadFunctionRefs
   ) where

import Ideas.Common.Environment
import Ideas.Common.Utils
import Data.Maybe
import Data.Typeable
import Control.Monad.State
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