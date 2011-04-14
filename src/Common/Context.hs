{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
   , newContext, getEnvironment, modifyEnvironment
     -- * Key-value pair environment (abstract)
   , Environment, emptyEnv, nullEnv, keysEnv, lookupEnv, storeEnv
   , diffEnv, deleteEnv
     -- * Variables
   , Var, newVar, makeVar
     -- * Lifting
   , liftToContext, liftTransContext
   , use, useC, termNavigator, applyTop
     -- * Context Monad
   , ContextMonad, readVar, writeVar, modifyVar
   , maybeCM, withCM, evalCM
   ) where 

import Common.Navigator
import Common.Rewriting
import Common.Transformation
import Common.Utils (commaList, readM)
import Common.View
import Control.Monad
import Data.Maybe
import Data.Dynamic
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
      let rest | null (keysEnv env) = "" 
               | otherwise = "  {" ++ show env ++ "}"
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

----------------------------------------------------------
-- Key-value pair environment (abstract)

newtype Environment = Env { envMap :: M.Map String (Maybe Dynamic, String) }

instance Show Environment where
   show = 
      let f (k, (_, v)) = k ++ "=" ++ v
      in commaList . map f . M.toList . envMap

emptyEnv :: Environment
emptyEnv = Env M.empty

nullEnv :: Environment -> Bool
nullEnv = null . keysEnv

keysEnv :: Environment -> [String]
keysEnv = M.keys . envMap

lookupEnv :: Typeable a => String -> Environment -> Maybe a
lookupEnv s (Env m) = result
 where
   result -- Special case for result type String
    | typeOf result == typeOf (Just "") = do
         (_, txt) <- M.lookup s m
         cast txt
    | otherwise = do
         (md, _) <- M.lookup s m
         d <- md
         fromDynamic d

storeEnv :: (Typeable a, Show a) => String -> a -> Environment -> Environment
storeEnv = storeEnvWith show

-- Generalized helper-function
storeEnvWith :: Typeable a => (a -> String) -> String -> a -> Environment -> Environment
storeEnvWith f s a (Env m) = Env (M.insert s pair m) 
 where -- Special case for type String
   pair = 
      case cast a of 
         Just txt -> (Nothing, txt)
         Nothing  -> (Just (toDyn a), f a)

diffEnv :: Environment -> Environment -> Environment
diffEnv (Env m1) (Env m2) = Env (M.filterWithKey p m1)
 where p k (_, s) = maybe True ((/=s) . snd) (M.lookup k m2)

deleteEnv :: String -> Environment -> Environment
deleteEnv s (Env m) = Env (M.delete s m)

----------------------------------------------------------
-- Variables

-- | A variable has a name and a default value (for initializing). Each
-- stored value must be readable and showable.
data Var a = V 
   { varName    :: String
   , varInitial :: a
   , varShow    :: a -> String
   , varRead    :: String -> Maybe a
   }

-- | Simple constructor function for creating a variable. Uses the 
-- Show and Read type classes
newVar :: (Show a, Read a) => String -> a -> Var a
newVar = makeVar show readM

-- | Extended constructor function for creating a variable. The show
-- and read functions are supplied explicitly.
makeVar :: (a -> String) -> (String -> Maybe a) -> String -> a -> Var a
makeVar showF readF s a = V s a showF readF

----------------------------------------------------------
-- Lifting rewrite rules

-- | Lift a rule to operate on a term in a context
liftToContext :: Rule a -> Rule (Context a)
liftToContext = liftRuleIn contextView

liftTransContext :: Transformation a -> Transformation (Context a)
liftTransContext = liftTransIn contextView

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

use :: (IsTerm a, IsTerm b) => Rule a -> Rule (Context b)
use = useC . liftToContext

useC :: (IsTerm a, IsTerm b) => Rule (Context a) -> Rule (Context b)
useC = liftRule (makeView (castT termView) (fromJust . castT termView))

contextView :: View (Context a) (a, Context a)
contextView = newView "views.contextView" f g
 where
   f ctx = current ctx >>= \a -> Just (a, ctx)
   g = uncurry replace

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

readVar :: Typeable a => Var a -> ContextMonad a
readVar var = CM $ \env -> return $
   let name = varName var
       txt  = fromMaybe "" $ lookupEnv name env
   in case (lookupEnv name env, varRead var txt) of
         (Just a, _) -> (a, env)
         (_, Just a) -> (a, storeEnvWith (varShow var) name a env)
         _           -> (varInitial var, env)

writeVar  :: Typeable a => Var a -> a -> ContextMonad ()
writeVar var a = 
   let f = storeEnvWith (varShow var) (varName var) a
   in CM $ \env -> return ((), f env)

modifyVar :: Typeable a => Var a -> (a -> a) -> ContextMonad ()
modifyVar var f = readVar var >>= (writeVar var  . f)

maybeCM :: Maybe a -> ContextMonad a
maybeCM = maybe mzero return