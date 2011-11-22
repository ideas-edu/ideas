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
-- Arguments and argument descriptors
--
-----------------------------------------------------------------------------
module Common.Argument
   ( -- * Arguments
     ArgDescr, labelArgument, defaultArgument, parseArgument, showArgument
   , termViewArgument, genArgument
   , defaultArgDescr, emptyArgDescr, simpleArgDescr
   , Argument(..), ArgValue(..), ArgValues, fromArgValue
   ) where

import Common.Rewriting
import Common.Utils
import Common.View
import Data.Typeable
import Test.QuickCheck

-----------------------------------------------------------
--- Arguments

-- | A data type for describing an argument of a parameterized transformation
data ArgDescr a = ArgDescr
   { labelArgument    :: String               -- ^ Label that is shown to the user when asked to supply the argument
   , defaultArgument  :: a                    -- ^ Default value that can be used
   , parseArgument    :: String -> Maybe a    -- ^ A parser
   , showArgument     :: a -> String          -- ^ A pretty-printer
   , termViewArgument :: Maybe (View Term a)  -- ^ Conversion to/from term
   , genArgument      :: Gen a                -- ^ An arbitrary argument generator
   , castArgument     :: forall b . Typeable b => a -> Maybe b
   }

-- | An argument descriptor, paired with a value
data ArgValue = forall a . ArgValue (ArgDescr a) a

fromArgValue :: Typeable a => ArgValue -> Maybe a
fromArgValue (ArgValue d a) = castArgument d a

-- | List of argument values
type ArgValues = [ArgValue]

instance Show ArgValue where
   show (ArgValue descr a) = labelArgument descr ++ "=" ++ showArgument descr a

instance Eq ArgValue where
   ArgValue d1 a1 == ArgValue d2 a2 =
      case (termViewArgument d1, termViewArgument d2) of
         (Just f, Just g)   -> build f a1 == build g a2
         (Nothing, Nothing) -> showArgument d1 a1 == showArgument d2 a2
         _                  -> False

-- | Constructor function for an argument descriptor that default type classes
defaultArgDescr :: (Show a, Read a, IsTerm a, Arbitrary a, Typeable a) => String -> a -> ArgDescr a
defaultArgDescr s a = ArgDescr s a readM show (Just termView) arbitrary cast

emptyArgDescr :: Typeable a => String -> a -> (a -> String) -> ArgDescr a
emptyArgDescr s a f = 
   ArgDescr s a (const Nothing) f Nothing (return a) cast

simpleArgDescr :: (Show a, Read a, Typeable a) => String -> a -> ArgDescr a 
simpleArgDescr s a = (emptyArgDescr s a show) {parseArgument = readM}

-- | A type class for types which have an argument descriptor
class Arbitrary a => Argument a where
   makeArgDescr :: String -> ArgDescr a   -- ^ The first argument is the label of the argument descriptor

instance Argument Int where
   makeArgDescr = flip defaultArgDescr 0
   
instance Argument Term where
   makeArgDescr s = ArgDescr s (TNum 0) (const Nothing) show (Just termView) arbitrary cast
   
instance Argument String where
   makeArgDescr s = ArgDescr s "" Just id Nothing arbitrary cast