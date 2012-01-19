{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, GADTs #-}
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
-- References, bindings, and heterogenous environments
--
-----------------------------------------------------------------------------
module Common.Binding
   ( -- * Reference 
     Ref, Reference(..)
     -- * Binding
   , Binding, makeBinding
   , fromBinding, showValue, getTermValue
     -- * Heterogeneous environment
   , Environment, makeEnvironment, singleBinding
   , HasEnvironment(..)
   , (?), bindings, noBindings
   ) where

import Common.Id
import Common.Rewriting.Term
import Common.Utils
import Common.View
import Control.Monad
import Data.Function
import Data.List
import Data.Monoid
import Data.Typeable
import qualified Data.Map as M
import Control.Monad.State

-----------------------------------------------------------
-- Reference

-- | A data type for references (without a value)
data Ref a = Ref
   { identifier :: Id                -- ^ Identifier
   , printer    :: a -> String       -- ^ A pretty-printer
   , _parser    :: String -> Maybe a -- ^ A parser
   , refView    :: View Term a       -- ^ Conversion to/from term
   }

instance Show (Ref a) where
   show = showId

instance Eq (Ref a) where
   (==) = (==) `on` getId

instance HasId (Ref a) where
   getId = identifier
   changeId f d = d {identifier = f (identifier d)}

-- | A type class for types as references
class (IsTerm a, Typeable a, Show a, Read a) => Reference a where
   makeRef     :: IsId n => n -> Ref a
   makeRefList :: IsId n => n -> Ref [a]
   -- default implementation
   makeRef n     = Ref (newId n) show readM termView
   makeRefList n = Ref (newId n) show readM termView

instance Reference Int
instance Reference Term

instance Reference Char where
   makeRefList n = Ref (newId n) id Just variableView

instance Reference a => Reference [a] where
   makeRef = makeRefList

-----------------------------------------------------------
-- Binding

data Binding = forall a . Typeable a => Binding (Ref a) a

instance Show Binding where 
   show a = showId a ++ "=" ++ showValue a
   
instance Eq Binding where
   (==) = let f (Binding ref a) = (getId ref, build (refView ref) a)
          in (==) `on` f
          
instance HasId Binding where
   getId (Binding ref _ ) = getId ref
   changeId f (Binding ref a) = Binding (changeId f ref) a

makeBinding :: Typeable a => Ref a -> a -> Binding
makeBinding = Binding

getTypedValue :: Typeable a => Binding -> Maybe a
getTypedValue (Binding _ a) =
   -- typed value
   cast a {- 
 `mplus` do
   -- value as string
   join $ liftM2 parser (gcast ref) (cast a) 
 `mplus` do
   -- value as term
   join $ liftM2 (match . refView) (gcast ref) (cast a) -}

fromBinding :: Typeable a => Binding -> Maybe (Ref a, a)
fromBinding (Binding ref a) = liftM2 (,) (gcast ref) (cast a)

showValue :: Binding -> String
showValue (Binding ref a) = printer ref a

getTermValue :: Binding -> Term
getTermValue (Binding ref a) = build (refView ref) a

-----------------------------------------------------------
-- Heterogeneous environment

newtype Environment = Env { envMap :: M.Map Id Binding }
   deriving Eq

instance Show Environment where
   show = intercalate ", " . map show . bindings

instance Monoid Environment where
   mempty = Env mempty
   mappend a b = Env (envMap a `mappend` envMap b) -- left has presedence

makeEnvironment :: [Binding] -> Environment
makeEnvironment xs = Env $ M.fromList [ (getId a, a) | a <- xs ]

singleBinding :: Typeable a => Ref a -> a -> Environment
singleBinding ref = makeEnvironment . return . Binding ref

class HasEnvironment env where 
   environment :: env -> Environment 
   deleteRef   :: Ref a -> env -> env
   insertRef   :: Typeable a => Ref a -> a -> env -> env
   changeRef   :: Typeable a => Ref a -> (a -> a) -> env -> env
   -- default definition
   changeRef ref f env  =
      maybe id (insertRef ref . f) (ref ? env) env

instance HasEnvironment Environment where
   environment    = id
   deleteRef a   = Env . M.delete (getId a) . envMap
   insertRef ref =
      let f a = Env . M.insert (getId a) a . envMap
      in f . Binding ref
   
(?) :: (HasEnvironment env, Typeable a) => Ref a -> env -> Maybe a
ref ? env =
   M.lookup (getId ref) (envMap (environment env)) >>= getTypedValue

bindings :: HasEnvironment env => env -> [Binding]
bindings = sortBy compareId . M.elems . envMap . environment

noBindings :: HasEnvironment env => env -> Bool
noBindings = M.null . envMap . environment

{-
data M m a where 
   M    :: StateT Environment m a -> M m a
   (:=) :: Typeable a => Ref a -> a -> M m ()
   (:~) :: Typeable a => Ref a -> (a -> a) -> M m ()

unM :: Monad m => M m a -> StateT Environment m a
unM (M s)      = s
unM (ref := a) = modify (insertRef ref a)
unM (ref :~ f) = modify (changeRef ref f)

instance Monad m => Monad (M m) where
   return a = M $ return a
   m >>= f  = M $ unM m >>= unM . f
   fail s   = M $ fail s

instance Monad m => MonadState Environment (M m) where
   get = M get
   put = M . put

getRef :: (Monad m, Typeable a) => Ref a -> M m a
getRef ref = M $ do
   env <- get
   maybe (fail "getRef") return (ref ? env)

runM :: Monad m => M m a -> Environment -> m (a, Environment)
runM = runStateT . unM

execM :: Monad m => M m a -> Environment -> m Environment
execM = execStateT . unM

evalM :: Monad m => M m a -> Environment -> m a
evalM = evalStateT . unM

s :: [M Maybe a] -> Environment -> Environment
s xs env = fromMaybe env (execM (sequence_ xs) env) -}