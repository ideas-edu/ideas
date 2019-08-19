{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
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
import Data.Semigroup as Sem
import Ideas.Common.Id
import Ideas.Common.Rewriting.Term
import Ideas.Common.View
import Ideas.Utils.Prelude
import Ideas.Utils.Typeable
import qualified Data.Map as M

-----------------------------------------------------------
-- Reference

-- | A data type for references (without a value)
data Ref a = Ref
   { identifier :: Id                -- Identifier
   , printer    :: a -> String       -- Pretty-printer
   , parser     :: String -> Maybe a -- Parser
   , refView    :: View Term a       -- Conversion to/from term
   , refType    :: IsTypeable a
   }

instance Show (Ref a) where
   show = showId

instance Eq (Ref a) where
   (==) = (==) `on` getId

instance HasId (Ref a) where
   getId = identifier
   changeId f d = d {identifier = f (identifier d)}

instance HasTypeable Ref where
   getTypeable = Just . refType

-- | A type class for types as references
class (IsTerm a, Typeable a, Show a, Read a) => Reference a where
   makeRef     :: IsId n => n -> Ref a
   makeRefList :: IsId n => n -> Ref [a]
   -- default implementation
   makeRef n     = Ref (newId n) show readM termView typeable
   makeRefList n = Ref (newId n) show readM termView typeable

instance Reference Int

instance Reference Term

instance Reference Char where
   makeRefList n = Ref (newId n) id Just variableView typeable

instance Reference ShowString

instance Reference a => Reference [a] where
   makeRef = makeRefList

instance (Reference a, Reference b) => Reference (a, b)

mapRef :: Typeable b => Isomorphism a b -> Ref a -> Ref b
mapRef iso r = r
   { printer = printer r . to iso
   , parser  = fmap (from iso) . parser r
   , refView = refView r >>> toView iso
   , refType = typeable
   }

-----------------------------------------------------------
-- Binding

data Binding = forall a . Binding (Ref a) a

instance Show Binding where
   show a = showId a ++ "=" ++ showValue a

instance Eq Binding where
   (==) = let f (Binding r a) = (getId r, build (refView r) a)
          in (==) `on` f

instance HasId Binding where
   getId (Binding r _ ) = getId r
   changeId f (Binding r a) = Binding (changeId f r) a

makeBinding :: Ref a -> a -> Binding
makeBinding = Binding

fromBinding :: Typeable a => Binding -> Maybe (Ref a, a)
fromBinding (Binding r a) = liftM2 (,) (gcastFrom r r) (castFrom r a)

showValue :: Binding -> String
showValue (Binding r a) = printer r a

getTermValue :: Binding -> Term
getTermValue (Binding r a) = build (refView r) a

-----------------------------------------------------------
-- Heterogeneous environment

newtype Environment = Env { envMap :: M.Map Id Binding }
   deriving Eq

instance Show Environment where
   show = intercalate ", " . map show . bindings

instance Sem.Semigroup Environment where
   a <> b = Env (envMap a `mappend` envMap b) -- left has presedence

instance Monoid Environment where
   mempty  = Env mempty
   mappend = (<>)

instance HasRefs Environment where
   allRefs env = [ Some ref | Binding ref _ <- bindings env ]

makeEnvironment :: [Binding] -> Environment
makeEnvironment xs = Env $ M.fromList [ (getId a, a) | a <- xs ]

singleBinding :: Ref a -> a -> Environment
singleBinding r = makeEnvironment . return . Binding r

class HasEnvironment env where
   environment    :: env -> Environment
   setEnvironment :: Environment -> env -> env
   deleteRef      :: Ref a -> env -> env
   insertRef      :: Ref a -> a -> env -> env
   changeRef      :: Ref a -> (a -> a) -> env -> env
   -- default definitions
   deleteRef a = changeEnv (Env . M.delete (getId a) . envMap)
   insertRef r =
      let f b = Env . M.insert (getId b) b . envMap
      in changeEnv . f . Binding r
   changeRef r f env  =
      maybe id (insertRef r . f) (r ? env) env

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
   Binding r a <- M.lookup (getId ref) m
   msum [ castBetween r ref a                  -- typed value
        , castFrom r a >>= parser ref          -- value as string
        , castFrom r a >>= match (refView ref) -- value as term
        ]