{-# LANGUAGE ExistentialQuantification #-}
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
   , ArgValue(..), ArgValues, fromArgValue
   ) where

import Common.Id
import Common.Rewriting.Term
import Common.Utils
import Common.View
import Data.Typeable

-----------------------------------------------------------
-- Binding data type 

-- | A data type for bindings (identifier and value)
data Binding a = Binding
   { identifier  :: Id                   -- ^ Identifier
   , value       :: a                    -- ^ Current value
   , printer     :: a -> String          -- ^ A pretty-printer
   , parser      :: String -> Maybe a    -- ^ A parser
   , bindingView :: Maybe (View Term a)  -- ^ Conversion to/from term
   }

instance HasId (Binding a) where
   getId = identifier
   changeId f d = d {identifier = f (identifier d)}

instance Show (Binding a) where
   show a = showId a ++ "=" ++ showValue a

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
n .<-. a = Binding (newId n) a show readM (Just termView)

stringBinding :: IsId n => n -> Binding String
stringBinding n = Binding (newId n) "" id Just (Just variableView)

termBinding :: IsId n => n -> Binding Term
termBinding n = Binding (newId n) (TNum 0) show (const Nothing) (Just termView)

-----------------------------------------------------------
-- Utility functions

showValue :: Binding a -> String
showValue a = printer a (value a)

getValue :: Binding a -> a
getValue = value

setValue :: a -> Binding a -> Binding a
setValue a b = b {value = a}

getTermValue :: Binding a -> Maybe Term
getTermValue a = fmap (`build` value a) (bindingView a)

readBinding :: Binding a -> String -> Maybe a
readBinding = parser

readTermBinding :: Binding a -> Term -> Maybe a
readTermBinding b term =
   bindingView b >>= (`match` term)

---------------------

-- | An argument descriptor, paired with a value
data ArgValue = forall a . Typeable a => ArgValue (Binding a)

fromArgValue :: Typeable a => ArgValue -> Maybe a
fromArgValue (ArgValue d) = cast (value d)

-- | List of argument values
type ArgValues = [ArgValue]

instance Show ArgValue where
   show (ArgValue a) = show a

instance Eq ArgValue where
   ArgValue d1 == ArgValue d2 =
      case (getTermValue d1, getTermValue d2) of
         (Just t1, Just t2) -> t1 == t2
         (Nothing, Nothing) -> showValue d1 == showValue d2
         _                  -> False