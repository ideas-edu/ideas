{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- References, bindings, and heterogenous environments
--
-----------------------------------------------------------------------------

module Ideas.Common.Environment
   ( -- * Reference
     Ref, Reference(..), mapRef
     -- * Binding
   , Binding, makeBinding
   , fromBinding, showValue, getTermValue
     -- * Heterogeneous environment
   , Environment, makeEnvironment, singleBinding
   , HasEnvironment(..), HasRefs(..)
   , bindings, noBindings, (?)
   ) where

import Control.Monad
import Data.Function
import Data.List
import Data.Typeable
import Ideas.Common.Id
import Ideas.Common.Rewriting.Term
import Ideas.Common.Utils
import Ideas.Common.View
import qualified Data.Map as M

-----------------------------------------------------------
-- Reference

-- | A data type for references (without a value)
data Ref a = Ref
   { identifier :: Id                -- Identifier
   , printer    :: a -> String       -- Pretty-printer
   , parser     :: String -> Maybe a -- Parser
   , refView    :: View Term a       -- Conversion to/from term
   , refCast    :: forall b . Typeable b => b -> Maybe a -- Wrapped 'cast' function
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
   makeRef n     = Ref (newId n) show readM termView cast
   makeRefList n = Ref (newId n) show readM termView cast

instance Reference Int

instance Reference Term

instance Reference Char where
   makeRefList n = Ref (newId n) id Just variableView cast

instance Reference ShowString

instance Reference a => Reference [a] where
   makeRef = makeRefList

instance (Reference a, Reference b) => Reference (a, b)

mapRef :: Typeable b => Isomorphism a b -> Ref a -> Ref b
mapRef iso r = r
   { printer = printer r . to iso
   , parser  = fmap (from iso) . parser r
   , refView = refView r >>> toView iso
   , refCast = cast
   }

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

instance HasRefs Environment where
   allRefs env = [ Some ref | Binding ref _ <- bindings env ]

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

class HasRefs a where
   getRefs   :: a -> [Some Ref]
   allRefs   :: a -> [Some Ref] -- with duplicates
   getRefIds :: a -> [Id]
   -- default implementation
   getRefIds a = [ getId r | Some r <- getRefs a]
   getRefs = sortBy cmp . nubBy eq . allRefs
    where
      cmp :: Some Ref -> Some Ref -> Ordering
      cmp (Some x) (Some y) = compareId (getId x) (getId y)
      eq a b = cmp a b == EQ

instance HasEnvironment Environment where
   environment    = id
   setEnvironment = const

bindings :: HasEnvironment env => env -> [Binding]
bindings = sortBy compareId . M.elems . envMap . environment

noBindings :: HasEnvironment env => env -> Bool
noBindings = M.null . envMap . environment

(?) :: HasEnvironment env => Ref a -> env -> Maybe a
ref ? env = do
   let m = envMap (environment env)
   Binding _ a <- M.lookup (getId ref) m
   msum [ refCast ref a                  -- typed value
        , cast a >>= parser ref          -- value as string
        , cast a >>= match (refView ref) -- value as term
        ]