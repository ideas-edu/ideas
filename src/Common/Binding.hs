{-# LANGUAGE FlexibleInstances #-}
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
-- Bindings
--
-----------------------------------------------------------------------------
module Common.Binding
   ( -- * Binding data type 
     Binding
     -- * Constructor and type class
   , Bindable(..), makeBindingWith, (.<-.)
   , stringBinding, termBinding
     -- * Utility functions
   , getValue, setValue, showValue, getTermValue 
   , readBinding, readTermBinding  
     -- * Heterogeneous environment
   , Environment, makeEnvironment, singleBinding
   , bindings, noBindings
   , insertBinding, insertTypedBinding, deleteBinding
   , lookupValue
   , Typed(..), HasEnvironment(..), (?)
   ) where

import Control.Monad
import Common.Id
import Common.Rewriting.Term
import Common.Utils
import Common.View
import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Map as M
import Data.Typeable

-----------------------------------------------------------
-- Binding data type 

-- | A data type for bindings (identifier and value)
data Binding a = Binding
   { identifier  :: Id                -- ^ Identifier
   , value       :: a                 -- ^ Current value
   , printer     :: a -> String       -- ^ A pretty-printer
   , parser      :: String -> Maybe a -- ^ A parser
   , bindingView :: View Term a       -- ^ Conversion to/from term
   }

instance Show (Binding a) where
   show a = showId a ++ "=" ++ showValue a

instance Eq (Binding a) where
   (==) = let f a = (getId a, getTermValue a)
          in (==) `on` f

instance HasId (Binding a) where
   getId = identifier
   changeId f d = d {identifier = f (identifier d)}

instance Show (Typed Binding) where
   show (Typed a) = show a

instance Eq (Typed Binding) where
   Typed a == Typed b = maybe False (==b) (gcast a)

instance HasId (Typed Binding) where
   getId (Typed a) = getId a
   changeId f (Typed a) = Typed (changeId f a)

-----------------------------------------------------------
-- Constructor and type class

-- | A type class for types with bindings
class Typeable a => Bindable a where
   makeBinding :: IsId n => n -> Binding a

instance Bindable Int where
   makeBinding = makeBindingWith 0

-- | Construct a new binding
makeBindingWith :: (Show a, Read a, IsTerm a, IsId n) => a -> n -> Binding a
makeBindingWith = flip (.<-.)

(.<-.) :: (Show a, Read a, IsTerm a, IsId n) => n -> a -> Binding a
n .<-. a = Binding (newId n) a show readM termView

stringBinding :: IsId n => n -> Binding String
stringBinding n = Binding (newId n) "" id Just variableView

termBinding :: IsId n => n -> Binding Term
termBinding n = Binding (newId n) (TNum 0) show (const Nothing) termView

-----------------------------------------------------------
-- Utility functions

showValue :: Binding a -> String
showValue a = printer a (value a)

getValue :: Binding a -> a
getValue = value

setValue :: a -> Binding a -> Binding a
setValue a b = b {value = a}

getTermValue :: Binding a -> Term
getTermValue a = build (bindingView a) (value a)

readBinding :: Binding a -> String -> Maybe a
readBinding = parser

readTermBinding :: Binding a -> Term -> Maybe a
readTermBinding = match . bindingView

getTypedValue :: Typeable a => Typed Binding -> Maybe a
getTypedValue (Typed b) = msum
   [ cast (getValue b) -- typed value
   , join $ liftM2 readBinding     (gcast b) (cast (getValue b)) -- value as string
   , join $ liftM2 readTermBinding (gcast b) (cast (getValue b)) -- value as term
   ]

-----------------------------------------------------------
-- Heterogeneous environment

newtype Environment = Env { envMap :: M.Map Id (Typed Binding) }
   deriving Eq

instance Show Environment where
   show = intercalate ", " . map show . bindings

instance Monoid Environment where
   mempty = Env mempty
   mappend a b = Env (envMap a `mappend` envMap b) -- left has presedence

class HasEnvironment env where 
   environment :: env -> Environment 

instance HasEnvironment Environment where
   environment = id

(?) :: (HasEnvironment env, HasId n, Typeable a) => n -> env -> Maybe a
(?) n = lookupValue n . environment

bindings :: HasEnvironment env => env -> [Typed Binding]
bindings = sortBy compareId . M.elems . envMap . environment

noBindings :: HasEnvironment env => env -> Bool
noBindings = M.null . envMap . environment

makeEnvironment :: [Typed Binding] -> Environment
makeEnvironment xs = Env $ M.fromList [ (getId a, a) | a <- xs ]

singleBinding :: Typeable a => Binding a -> Environment
singleBinding = makeEnvironment . return . Typed

insertTypedBinding :: Typed Binding -> Environment -> Environment
insertTypedBinding a = Env . M.insert (getId a) a . envMap

insertBinding :: Typeable a => Binding a -> Environment -> Environment
insertBinding = insertTypedBinding . Typed

deleteBinding :: HasId a => a -> Environment -> Environment
deleteBinding a = Env . M.delete (getId a) . envMap

lookupTypedBinding :: HasId a => a -> Environment -> Maybe (Typed Binding)
lookupTypedBinding a = M.lookup (getId a) . envMap

lookupValue :: (Typeable a, HasId b) => b -> Environment -> Maybe a
lookupValue a env = lookupTypedBinding a env >>= getTypedValue