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
module Common.Environment
   ( -- * Reference 
     Ref, Reference(..)
     -- * Binding
   , Binding, makeBinding
   , fromBinding, showValue, getTermValue
     -- * Heterogeneous environment
   , Environment, makeEnvironment, singleBinding
   , HasEnvironment(..)
   , bindings, noBindings, (?)
     -- * Environment Monad
   , EnvMonad, EnvMonadT((:=), (:~), (:?))
   , getRef, updateRefs
   , runEnvMonad, execEnvMonad, evalEnvMonad
   , envMonadRefs, envMonadFunctionRefs
   ) where

import Common.Id
import Common.Rewriting.Term
import Common.Utils
import Common.View
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable
import qualified Data.Map as M
import Control.Monad.State
import qualified Control.Exception as C

-----------------------------------------------------------
-- Reference

-- | A data type for references (without a value)
data Ref a = Ref
   { identifier :: Id                -- ^ Identifier
   , printer    :: a -> String       -- ^ A pretty-printer
   , parser     :: String -> Maybe a -- ^ A parser
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
   environment    :: env -> Environment 
   setEnvironment :: Environment -> env -> env
   deleteRef      :: Ref a -> env -> env
   insertRef      :: Typeable a => Ref a -> a -> env -> env
   changeRef      :: Typeable a => Ref a -> (a -> a) -> env -> env
   -- default definitions
   deleteRef a = changeEnv (Env . M.delete (getId a) . envMap)
   insertRef ref =
      let f b = Env . M.insert (getId b) b . envMap
      in changeEnv . f . Binding ref
   changeRef ref f env  =
      maybe id (insertRef ref . f) (ref ? env) env

-- local helper
changeEnv :: HasEnvironment env => (Environment -> Environment) -> env -> env
changeEnv f env = setEnvironment (f (environment env)) env

instance HasEnvironment Environment where
   environment    = id
   setEnvironment = const
      
bindings :: HasEnvironment env => env -> [Binding]
bindings = sortBy compareId . M.elems . envMap . environment

noBindings :: HasEnvironment env => env -> Bool
noBindings = M.null . envMap . environment

(?) :: (HasEnvironment env, Typeable a) => Ref a -> env -> Maybe a
ref ? env = do
   let m = envMap (environment env)
   Binding _ a <- M.lookup (getId ref) m
   msum [ cast a                         -- typed value
        , cast a >>= parser ref          -- value as string
        , cast a >>= match (refView ref) -- value as term
        ]

-----------------------------------------------------------
-- Environment Monad

type EnvMonad = EnvMonadT Maybe

infix 2 :=, :~, :?

data EnvMonadT m a where 
   -- Monad operations
   Return :: a -> EnvMonadT m a
   Bind   :: EnvMonadT m a -> (a -> EnvMonadT m b) -> EnvMonadT m b
   Then   :: EnvMonadT m a -> EnvMonadT m b -> EnvMonadT m b
   Fail   :: String -> EnvMonadT m b
   -- MonadPlus operations
   Zero   :: MonadPlus m => EnvMonadT m a
   Plus   :: MonadPlus m => EnvMonadT m a -> EnvMonadT m a -> EnvMonadT m a
   -- References (special)
   (:=)   :: Typeable a => Ref a -> a -> EnvMonadT m ()
   (:~)   :: Typeable a => Ref a -> (a -> a) -> EnvMonadT m ()
   (:?)   :: Typeable a => Ref a -> a -> EnvMonadT m a
   GetRef :: Typeable a => Ref a -> EnvMonadT m a

{-
unM :: Monad m => EnvMonadT m a -> StateT Environment m a
unM (M s)      = s
unM (Bind m f) = unM m >>= unM . f
unM (Plus a b) = unM a `mplus` unM b
unM (ref := a) = modify (insertRef ref a)
unM (ref :~ f) = modify (changeRef ref f)
unM (ref :? a) = gets (fromMaybe a . (ref ?)) -}

instance Monad m => Monad (EnvMonadT m) where
   return = Return
   (>>=)  = Bind
   fail   = Fail

instance MonadPlus m => MonadPlus (EnvMonadT m) where
   mzero = Zero
   mplus = Plus

getRef :: (Monad m, Typeable a) => Ref a -> EnvMonadT m a
getRef = GetRef {- ref = M $ do
   env <- get
   maybe (fail "getRef") return (ref ? env) -}

updateRefs :: [EnvMonad a] -> Environment -> Environment
updateRefs xs env = fromMaybe env (execEnvMonad (sequence_ xs) env)

runEnvMonad :: Monad m => EnvMonadT m a -> Environment -> m (a, Environment)
runEnvMonad = runStateT . rec
 where
   rec :: Monad m => EnvMonadT m a -> StateT Environment m a
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

execEnvMonad :: Monad m => EnvMonadT m a -> Environment -> m Environment
execEnvMonad m = liftM snd . runEnvMonad m

evalEnvMonad :: Monad m => EnvMonadT m a -> Environment -> m a
evalEnvMonad m = liftM fst . runEnvMonad m

envMonadRefs :: EnvMonadT m a -> IO [Some Ref]
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

envMonadFunctionRefs :: (a -> EnvMonadT m b) -> IO [Some Ref]
envMonadFunctionRefs f = envMonadRefs (f (error "catch me")) 
   `C.catch` \(C.ErrorCall _) -> return []

{-
test :: EnvMonad Int 
test = do
   (r1 := "ok") >>
    ((r3 := "ok") >>
    ((r1 := "ok") >>
     undefined >>
    ((r1 := "no") >>
    ((r3 := "yes") >>
    Return 0))))
   a <- ((r3 := "law") >>= (\a -> r4 := "diep")) >>= \b -> r4 := "diepst"
   guard False
   x <- do 
      if (False) then r2 :~ (^2) else return ()
      n <- r2 :? 0
      return n
   r3 := "fout"	
   return 0
   -- if n==0 then return (n+1) else return (-1)
 `mplus` do
   r3 := "fout"
   return 0
   
r1, r3, r4 :: Ref String
r1 = makeRef "r1"
r3 = makeRef "r3"
r4 = makeRef "r4"

r2 :: Ref Int
r2 = makeRef "r2"

go = liftM (map (\(Some r) -> show r)) (envMonadRefs test) -}