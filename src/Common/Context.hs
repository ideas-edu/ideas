{-# OPTIONS -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
     Context, inContext, update, replace, fromContext, makeContext, getEnvironment
     -- * Key-value pair environment (abstract)
   , Environment, emptyEnv, nullEnv, keysEnv, lookupEnv, storeEnv
   , diffEnv, deleteEnv
     -- * Variables
   , Var, newVar, makeVar
     -- * Location (current focus)
   , Location, location, setLocation, changeLocation
   , currentFocus, changeFocus, locationDown, locationUp
   , makeLocation, fromLocation
     -- * Lifting
   , liftToContext, ignoreContext, liftTransContext, contextView
     -- * Context Monad
   , ContextMonad, runCM, readVar, writeVar, modifyVar
   , maybeCM, withCM, evalCM -- , listCM, runListCM, withListCM
   ) where

import Common.Transformation
import Common.Uniplate
import Common.Utils (safeHead, commaList, readM)
import Common.View
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Dynamic
import Data.List
import Test.QuickCheck
import qualified Data.Map as M

----------------------------------------------------------
-- Abstract data type

-- | Abstract data type for a context: a context stores an envrionent 
-- (key-value pairs) and a value
data Context a = C 
   { getEnvironment :: Environment -- ^ Returns the environment
   , fromContext    :: a           -- ^ Retrieve a value from its context
   } 

instance Eq a => Eq (Context a) where
   x == y = fromContext x == fromContext y

instance Ord a => Ord (Context a) where
   x `compare` y = fromContext x `compare` fromContext y

instance Show a => Show (Context a) where
   show (C env a) = 
      let rest | null (keysEnv env) = "" 
               | otherwise = "  {" ++ show env ++ "}"
      in show a ++ rest 

instance Arbitrary a => Arbitrary (Context a) where
   arbitrary   = liftM inContext arbitrary
   coarbitrary = coarbitrary . fromContext

-- | Construct a context
makeContext :: Environment -> a -> Context a
makeContext = C

-- | Put a value into a (default) context
inContext :: a -> Context a
inContext = makeContext emptyEnv

replace :: a -> Context a -> Context a
replace = update . const

update :: (a -> a) -> Context a -> Context a
update = contextMap

contextMap f (C env a) = C env (f a)

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
-- Location (current focus)

locationVar :: Var Location
locationVar = newVar "location" emptyLocation

-- | Type synonym for the current location (focus)
newtype Location = L [Int] deriving (Eq, Ord, Typeable)

instance Show Location where
   show (L is) = show is
   
instance Read Location where
   readsPrec n s = [ (L is, rest) | (is, rest) <- readsPrec n s ]

-- | Returns the current location of a context
location :: Context a -> Location
location = fromMaybe emptyLocation . evalCM (const (readVar locationVar))

-- | Replaces the current location of a context
setLocation :: Location -> Context a -> Context a 
setLocation loc c = fromMaybe c $ withCM f c
 where f a = writeVar locationVar loc >> return a

-- | Updates the current location of a context
changeLocation :: (Location -> Location) -> Context a -> Context a
changeLocation f c = setLocation (f (location c)) c

-- | Returns the term which has the current focus: Nothing indicates that the current 
-- focus is invalid
currentFocus :: Uniplate a => Context a -> Maybe a
currentFocus c = getTermAt (fromLocation $ location c) (fromContext c)

-- | Changes the term which has the current focus. In case the focus is invalid, then
-- this function has no effect.
changeFocus :: Uniplate a => (a -> a) -> Context a -> Context a
changeFocus f c = update (applyAt (fromLocation $ location c) f) c

-- | Go down to a certain child
locationDown :: Int -> Location -> Location
locationDown i (L is) = L (is ++ [i])

-- | Go up: Nothing indicates that we were already at the top
locationUp :: Location -> Maybe Location
locationUp (L is)
   | null is   = Nothing
   | otherwise = Just (L (init is))

makeLocation :: [Int] -> Location
makeLocation = L

fromLocation :: Location -> [Int]
fromLocation (L is) = is

emptyLocation :: Location
emptyLocation = makeLocation []

----------------------------------------------------------
-- Lifting rewrite rules

-- | Lift a rule to operate on a term in a context
liftToContext :: Uniplate a => Rule a -> Rule (Context a)
liftToContext = liftRuleIn (makeView f g)
 where
   f ctx = currentFocus ctx >>= \a -> Just (a, ctx)
   g (a, ctx) = changeFocus (const a) ctx

-- | Lift a rule to operate on a term in a context by ignoring the context
ignoreContext :: Rule a -> Rule (Context a)
ignoreContext = liftRuleIn ignoreContextView

liftTransContext :: Transformation a -> Transformation (Context a)
liftTransContext = liftTransIn ignoreContextView

ignoreContextView :: View (Context a) (a, Environment)
ignoreContextView = makeView f g
 where 
   f c      = Just (fromContext c, getEnvironment c) 
   g (a, e) = makeContext e a

switchContext :: Monad m => Context (m a) -> m (Context a)
switchContext (C env ma) = liftM (C env) ma

contextView :: Monad m => ViewM m a b -> ViewM m (Context a) (Context b)
contextView v = makeView (switchContext . contextMap (match v)) (contextMap (build v))

----------------------------------------------------------
-- Context monad

newtype ContextMonad a = CM (Environment -> [(a, Environment)])

withCM :: (a -> ContextMonad b) -> Context a -> Maybe (Context b)
withCM f c = runCM (f (fromContext c)) (getEnvironment c)

evalCM :: (a -> ContextMonad b) -> Context a -> Maybe b
evalCM f = fmap fromContext . withCM f

runCM :: ContextMonad a -> Environment -> Maybe (Context a)
runCM (CM f) env = do
   (a, e) <- safeHead (f env)
   return (C e a)

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


{-
listCM :: [a] -> ContextMonad a
listCM = foldr (mplus . return) mzero

withListCM :: (a -> ContextMonad b) -> Context a -> [Context b]
withListCM f c = runListCM (f (fromContext c)) (getEnvironment c)

runListCM :: ContextMonad a -> Environment -> [Context a]
runListCM (CM f) env = do
   (a, e) <- f env
   return (C e a) -}