{-# OPTIONS -XDeriveDataTypeable -XTypeSynonymInstances -XMultiParamTypeClasses #-}
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
-- A context for a term that maintains a current focus and an environment of
-- key-value pairs. A context is both showable and parsable.
--
-----------------------------------------------------------------------------
module Common.Context 
   ( -- * Abstract data type
     Context, inContext, fromContext, showContext, parseContext
   , makeContext, contextPairs -- new
     -- * Variable environment
   , Var, newVar, makeVar
--   , newIntVar, newIntegerVar, newBoolVar, newListVar 
--   , readVar, writeVar, modifyVar
--   , readV, writeV, modifyV
     -- * Location (current focus)
   , Location, location, setLocation, changeLocation
   , currentFocus, changeFocus, locationDown, locationUp
   , makeLocation, fromLocation
     -- * Lifting
   , liftToContext, ignoreContext
   , ContextMonad, runContextMonad, readV, writeV, modifyV
   , maybeCM, withCM, evalCM
   ) where

import Common.Transformation
import Common.Uniplate
import Common.Utils
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Dynamic
import Data.List
import Test.QuickCheck
import qualified Data.Map as M

----------------------------------------------------------
-- Abstract data type

-- | Abstract data type for a context: a context stores an envrionent (key-value pairs) and
-- a current focus (list of integers)
data Context a = C Env a

instance Eq a => Eq (Context a) where
   x == y = fromContext x == fromContext y

instance Ord a => Ord (Context a) where
   x `compare` y = fromContext x `compare` fromContext y

instance Show a => Show (Context a) where
   show c = showContext c ++ ";" ++ show (fromContext c)

instance Functor Context where
   fmap f (C env a) = C env (f a)

instance Arbitrary a => Arbitrary (Context a) where
   arbitrary   = liftM inContext arbitrary
   coarbitrary = coarbitrary . fromContext

-- | Put a value into a (default) context
inContext :: a -> Context a
inContext = C M.empty

-- | Retrieve a value from its context
fromContext :: Context a -> a
fromContext (C _ a) = a

getEnvironment :: Context a -> Env
getEnvironment (C env _) = env

setEnvironment :: Env -> Context ()
setEnvironment env = C env ()

----------------------------------------------------------
-- A simple parser and pretty-printer for contexts

-- | Shows the context (without the embedded value)
showContext :: Context a -> String
showContext = showEnv . getEnvironment

-- local helper function
showEnv :: Env -> String
showEnv = concat . intersperse "," . map f . M.toList
 where f (k, (_, v)) = k ++ "=" ++ v

-- | Parses a context: on a successful parse, the unit value is returned 
-- in the parsed context
parseContext :: String -> Maybe (Context ())
parseContext s
   | all isSpace s = 
        return $ setEnvironment M.empty
   | otherwise = do
        pairs <- mapM (splitAtElem '=') (splitsWithElem ',' s)
        let f (k, v) = (trim k, (Nothing, v))
        return $ setEnvironment (M.fromList $ map f pairs)
        
makeContext :: [(String, (Maybe Dynamic, String))] -> Context ()
makeContext = setEnvironment . M.fromList

contextPairs :: Context a -> [(String, (Maybe Dynamic, String))]
contextPairs = M.toList . getEnvironment

----------------------------------------------------------
-- Manipulating the variable environment

-- local type synonym: can probably be simplified
type Env = M.Map String (Maybe Dynamic, String)

-- | A variable has a name (for showing) and a default value (for initializing)
data Var a = V 
   { varName    :: String
   , varInitial :: a
   , varShow    :: a -> String
   , varRead    :: String -> Maybe a
   }

makeVar :: (a -> String) -> (String -> Maybe a) -> String -> a -> Var a
makeVar showF readF s a = V s a showF readF

newVar :: (Show a, Read a) => String -> a -> Var a
newVar = makeVar show defaultRead

defaultRead :: Read a => String -> Maybe a
defaultRead s = 
   case reads s of
      [(b, rest)] | all isSpace rest -> Just b
      _ -> Nothing      

-- | Returns the value of a variable stored in a context
readVar :: Typeable a => Var a -> Context b -> a
readVar var c = 
   -- if something goes wrong, return the initial value
   fromMaybe (varInitial var) $ do
   pair <- M.lookup (varName var) (getEnvironment c)
   case pair of
      (Just d, _) -> -- use the stored dynamic (default value as backup)
         fromDynamic d
      (Nothing, s) -> -- parse the pretty-printed value (default value as backup)
         varRead var s

-- | Replaces the value of a variable stored in a context
writeVar :: Typeable a => Var a -> a -> Context b -> Context b
writeVar var a c = 
   let oldEnv = getEnvironment c 
       newEnv = M.insert (varName var) (Just (toDyn a), varShow var a) oldEnv
       value  = fromContext c
   in fmap (const value) (setEnvironment newEnv)

-- | Updates the value of a variable stored in a context
modifyVar :: Typeable a => Var a -> (a -> a) -> Context b -> Context b
modifyVar v f c = writeVar v (f (readVar v c)) c
 
----------------------------------------------------------
-- Location (current focus)

locationVar :: Var Location
locationVar = newVar "location" (makeLocation [])

-- | Type synonym for the current location (focus)
newtype Location = L [Int] deriving (Eq, Ord, Typeable)

instance Show Location where
   show (L is) = show is
   
instance Read Location where
   readsPrec n s = [ (L is, rest) | (is, rest) <- readsPrec n s ]

-- | Returns the current location of a context
location :: Context a -> Location
location = readVar locationVar

-- | Replaces the current location of a context
setLocation :: Location -> Context a -> Context a 
setLocation = writeVar locationVar

-- | Updates the current location of a context
changeLocation :: (Location -> Location) -> Context a -> Context a
changeLocation = modifyVar locationVar

-- | Returns the term which has the current focus: Nothing indicates that the current 
-- focus is invalid
currentFocus :: Uniplate a => Context a -> Maybe a
currentFocus c = getTermAt (fromLocation $ location c) (fromContext c)

-- | Changes the term which has the current focus. In case the focus is invalid, then
-- this function has no effect.
changeFocus :: Uniplate a => (a -> a) -> Context a -> Context a
changeFocus f c = fmap (applyAt (fromLocation $ location c) f) c

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

----------------------------------------------------------
-- Lifting rewrite rules

-- | Lift a rule to operate on a term in a context
liftToContext :: (Lift f, Uniplate a) => f a -> f (Context a)
liftToContext = lift $ makeLiftPair currentFocus (changeFocus . const)

-- | Lift a rule to operate on a term in a context by ignoring the context
ignoreContext :: Lift f => f a -> f (Context a)
ignoreContext = lift $ makeLiftPair (return . fromContext) (fmap . const)

----------------------------------------------------------
-- New: Context monad

newtype ContextMonad a = CM (Env -> Maybe (a, Env))

withCM :: (a -> ContextMonad b) -> Context a -> Maybe (Context b)
withCM f c = runContextMonad (f (fromContext c)) (getEnvironment c)

evalCM :: (a -> ContextMonad b) -> Context a -> Maybe b
evalCM f = fmap fromContext . withCM f

runContextMonad :: ContextMonad a -> Env -> Maybe (Context a)
runContextMonad (CM f) env = do
   (a, e) <- f env
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

readV   :: Typeable a => Var a -> ContextMonad a
readV var = make $ \env -> 
   case M.lookup (varName var) env of
      Just (Just d, _) -> -- use the stored dynamic
         fromDynamic d
      Just (Nothing, s) -> -- parse the pretty-printed value (default value as backup)
         varRead var s
      _ -> Nothing
 where
   make f = CM $ \env -> 
      return (fromMaybe (varInitial var) (f env), env)

writeV  :: Typeable a => Var a -> a -> ContextMonad ()
writeV var a = CM $ \env -> return ((), f env)
 where 
    pair = (Just (toDyn a), varShow var a)
    f    = M.insert (varName var) pair

modifyV :: Typeable a => Var a -> (a -> a) -> ContextMonad ()
modifyV var f = readV var >>= (writeV var  . f)

maybeCM :: Maybe a -> ContextMonad a
maybeCM = maybe mzero return

--listCM :: [a] -> ContextMonad a
--listCM xs = CM (zip xs . repeat)