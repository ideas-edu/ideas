{-# LANGUAGE DeriveDataTypeable #-}
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
-- A context for a term that maintains an environment of
-- key-value pairs. A context is both showable and parsable.
--
-----------------------------------------------------------------------------
module Common.Context
   ( -- * Abstract data type
     Context, fromContext, fromContextWith, fromContextWith2
   , newContext, getEnvironment, modifyEnvironment, getArgValues
     -- * Key-value pair environment (abstract)
   , Environment, emptyEnv, nullEnv
   , storeEnvString, storeEnvTerm, storeArg, lookupArg
   , diffEnv, deleteEnv
     -- * Lifting
   , liftToContext
   , use, useC, termNavigator, applyTop
     -- * Context Monad
   , ContextMonad, readVar, writeVar, modifyVar
   , maybeCM, withCM, evalCM
   ) where

import Common.Argument
import Common.Id
import Common.Navigator
import Common.Rewriting
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Data.Typeable
import qualified Data.Map as M

----------------------------------------------------------
-- Abstract data type

-- | Abstract data type for a context: a context stores an envrionent
-- (key-value pairs) and a value
data Context a = C
   { getEnvironment :: Environment -- ^ Returns the environment
   , getNavigator   :: Navigator a -- ^ Retrieve a value from its context
   }

fromContext :: Monad m => Context a -> m a
fromContext = leave . getNavigator

fromContextWith :: Monad m => (a -> b) -> Context a -> m b
fromContextWith f = liftM f . fromContext

fromContextWith2 :: Monad m => (a -> b -> c) -> Context a -> Context b -> m c
fromContextWith2 f a b = liftM2 f (fromContext a) (fromContext b)

instance Eq a => Eq (Context a) where
   x == y = fromMaybe False $ liftM2 (==) (fromContext x) (fromContext y)

instance Show a => Show (Context a) where
   show (C env a) =
      let rest | nullEnv env = ""
               | otherwise   = "  {" ++ show env ++ "}"
      in show a ++ rest

instance IsNavigator Context where
   up        (C env a) = liftM (C env) (up a)
   allDowns  (C env a) = map (C env) (allDowns a)
   current   (C _   a) = current a
   location  (C _   a) = location a
   changeM f (C env a) = liftM (C env) (changeM f a)

instance TypedNavigator Context where
   changeT f (C env a) = liftM (C env) (changeT f a)
   currentT  (C _   a) = currentT a
   leaveT    (C _   a) = leaveT a
   castT v   (C env a) = liftM (C env) (castT v a)

-- | Construct a context
newContext :: Environment -> Navigator a -> Context a
newContext = C

modifyEnvironment :: (Environment -> Environment) -> Context a -> Context a
modifyEnvironment f c = c {getEnvironment = f (getEnvironment c)}

getArgValues :: Context a -> ArgValues
getArgValues = M.elems . envMap . getEnvironment

----------------------------------------------------------
-- Key-value pair environment (abstract)

newtype Environment = Env { envMap :: M.Map String ArgValue }

instance Show Environment where
   show = intercalate ", " . map show . M.elems . envMap

showItem :: ArgValue -> String
showItem (ArgValue descr a) = showArgument descr a

emptyEnv :: Environment
emptyEnv = Env M.empty

nullEnv :: Environment -> Bool
nullEnv = M.null . envMap
 
lookupArg :: Typeable a => ArgDescr a -> Environment -> a
lookupArg descr (Env m) = 
   fromMaybe (defaultArgument descr) $ do 
      a <- M.lookup (labelArgument descr) m
      fromArgValue a `mplus`
         (fromArgValue a >>= parseArgument descr) `mplus`
         (join $ liftM2 match (termViewArgument descr) (fromArgValue a))

storeArg :: ArgDescr a -> a -> Environment -> Environment
storeArg descr a (Env m) = 
   Env (M.insert (labelArgument descr) (ArgValue descr a) m) 

storeEnvString :: String -> String -> Environment -> Environment
storeEnvString = storeArg . makeArgDescr

storeEnvTerm :: String -> Term -> Environment -> Environment
storeEnvTerm = storeArg . makeArgDescr

diffEnv :: Environment -> Environment -> Environment
diffEnv (Env m1) (Env m2) = Env (M.filterWithKey p m1)
 where p k a = maybe False ((/= showItem a) . showItem) (M.lookup k m2)

deleteEnv :: String -> Environment -> Environment
deleteEnv s (Env m) = Env (M.delete s m)

----------------------------------------------------------
-- Lifting rules

-- | Lift a rule to operate on a term in a context
liftToContext :: LiftView f => f a -> f (Context a)
liftToContext = liftViewIn cv
 where
   cv    = "views.contextView" @> makeView f g
   f ctx = current ctx >>= \a -> Just (a, ctx)
   g     = uncurry replace
   
-- | Apply a function at top-level. Afterwards, try to return the focus
-- to the old position
applyTop :: (a -> a) -> Context a -> Context a
applyTop f c =
   case top c of
      Just ok -> navigateTowards (location c) (change f ok)
      Nothing -> c

termNavigator :: IsTerm a => a -> Navigator a
termNavigator a = fromMaybe (noNavigator a) (make a)
 where
   make = castT termView . viewNavigatorWith spineHoles . toTerm

   spineHoles :: Term -> [(Term, Term -> Term)]
   spineHoles term
      | null xs   = []
      | otherwise = (x, flip makeTerm xs) : zipWith f [0..] xs
    where
      (x, xs)    = getSpine term
      f i y      = (y, makeTerm x . changeAt i)
      changeAt i b =
         case splitAt i xs of
            (ys, _:zs) -> ys ++ b:zs
            _          -> xs

use :: (LiftView f, IsTerm a, IsTerm b) => f a -> f (Context b)
use = useC . liftToContext

useC :: (LiftView f, IsTerm a, IsTerm b) => f (Context a) -> f (Context b)
useC = liftView (makeView (castT termView) (fromJust . castT termView))

----------------------------------------------------------
-- Context monad

newtype ContextMonad a = CM { unCM :: Environment -> Maybe (a, Environment) }

withCM :: (a -> ContextMonad a) -> Context a -> Maybe (Context a)
withCM f c = do
   a0       <- current c
   (a, env) <- unCM (f a0) (getEnvironment c)
   let nav = replace a (getNavigator c)
   return (newContext env nav)

evalCM :: (a -> ContextMonad b) -> Context a -> Maybe b
evalCM f c = do
   a0     <- current c
   (b, _) <- unCM (f a0) (getEnvironment c)
   return b

instance Functor ContextMonad where
   fmap = liftM

instance Monad ContextMonad where
   fail       = const mzero
   return a   = CM (\env -> return (a, env))
   CM m >>= f = CM (\env -> do (a, e) <- m env
                               let CM g = f a
                               g e)

instance MonadPlus ContextMonad where
   mzero = CM (const mzero)
   mplus (CM f) (CM g) = CM (\env -> f env `mplus` g env)

readVar :: Typeable a => ArgDescr a -> ContextMonad a
readVar var = CM $ \env -> Just (lookupArg var env, env)

writeVar  :: Typeable a => ArgDescr a -> a -> ContextMonad ()
writeVar var a = CM $ \env -> return ((), storeArg var a env)

modifyVar :: Typeable a => ArgDescr a -> (a -> a) -> ContextMonad ()
modifyVar var f = readVar var >>= (writeVar var  . f)

maybeCM :: Maybe a -> ContextMonad a
maybeCM = maybe mzero return