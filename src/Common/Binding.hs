{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeSynonymInstances #-}
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
   ( -- * Binding
     Binding, getValue, setValue, showValue, readBinding, bindingParser
   , bindingTermView, readTermBinding, getTermValue
   , defaultArgDescr, emptyArgDescr
   , Argument(..), ArgValue(..), ArgValues, fromArgValue
   ) where

import Common.Id
import Common.Rewriting.Term
import Common.Utils
import Common.View
import Data.Typeable

-----------------------------------------------------------
--- Arguments

-- | A data type for bindings (identifier and value)
data Binding a = Binding
   { identifier  :: Id                   -- ^ Identifier
   , value       :: a                    -- ^ Current value
   , parser      :: String -> Maybe a    -- ^ A parser
   , printer     :: a -> String          -- ^ A pretty-printer
   , bindingView :: Maybe (View Term a)  -- ^ Conversion to/from term
   }

instance HasId (Binding a) where
   getId = identifier
   changeId f d = d {identifier = f (identifier d)}

instance Show (Binding a) where
   show a = showId a ++ "=" ++ showValue a

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
readTermBinding b term = do
   v <- bindingView b
   match v term

bindingParser :: (String -> Maybe a) -> Binding a -> Binding a
bindingParser f b = b {parser = f}

bindingTermView :: View Term a -> Binding a -> Binding a
bindingTermView v b = b {bindingView = Just v}

-- | Constructor function for an argument descriptor that default type classes
defaultArgDescr :: (Show a, Read a, IsTerm a, IsId n) => n -> a -> Binding a
defaultArgDescr n a = Binding (newId n) a readM show (Just termView)

emptyArgDescr :: IsId n => n -> a -> (a -> String) -> Binding a
emptyArgDescr n a f = 
   Binding (newId n) a (const Nothing) f Nothing

-- | A type class for types which have an argument descriptor
class Typeable a => Argument a where
   makeArgDescr :: IsId n => n -> Binding a

instance Argument Int where
   makeArgDescr = flip defaultArgDescr 0
   
instance Argument String where
   makeArgDescr n = Binding (newId n) "" Just id Nothing
   
instance Argument Term where
   makeArgDescr n = Binding (newId n) (TNum 0) (const Nothing) show (Just termView)
   
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