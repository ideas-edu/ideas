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
-- Arguments and argument descriptors
--
-----------------------------------------------------------------------------
module Common.Argument
   ( -- * Arguments
     ArgDescr, labelArgument, defaultArgument, parseArgument, showArgument
   , termViewArgument, genArgument
   , defaultArgDescr, newArgDescr, simpleArgDescr
   , Argument(..), ArgValue(..), ArgValues
   ) where

import Common.Rewriting
import Common.Utils
import Common.View
import Test.QuickCheck

-----------------------------------------------------------
--- Arguments

-- | A data type for describing an argument of a parameterized transformation
data ArgDescr a = ArgDescr
   { labelArgument    :: String               -- ^ Label that is shown to the user when asked to supply the argument
   , defaultArgument  :: a                    -- ^ Default value that can be used
   , parseArgument    :: String -> Maybe a    -- ^ A parser
   , showArgument     :: a -> String          -- ^ A pretty-printer
   , termViewArgument :: View Term a          -- ^ Conversion to/from term
   , genArgument      :: Gen a                -- ^ An arbitrary argument generator
   }

-- | An argument descriptor, paired with a value
data ArgValue = forall a . ArgValue (ArgDescr a) a

-- | List of argument values
type ArgValues = [ArgValue]

instance Show ArgValue where
   show (ArgValue descr a) = labelArgument descr ++ "=" ++ showArgument descr a

instance Eq ArgValue where
   ArgValue d1 a1 == ArgValue d2 a2 =
      build (termViewArgument d1) a1 == build (termViewArgument d2) a2

-- | Constructor function for an argument descriptor that uses the Show and Read type classes
defaultArgDescr :: (Show a, Read a, IsTerm a, Arbitrary a) => String -> a -> ArgDescr a
defaultArgDescr descr a = ArgDescr descr a readM show termView arbitrary

newArgDescr :: (IsTerm a, Arbitrary a) => String -> a -> (a -> String) -> (String -> Maybe a) -> ArgDescr a
newArgDescr descr a showF readF = ArgDescr descr a readF showF termView arbitrary

simpleArgDescr :: (Show a, Read a) => String -> a -> ArgDescr a
simpleArgDescr descr a = ArgDescr descr a readM show failV (return a)
 where failV = makeView (const Nothing) (const (TNum 0))

-- | A type class for types which have an argument descriptor
class Arbitrary a => Argument a where
   makeArgDescr :: String -> ArgDescr a   -- ^ The first argument is the label of the argument descriptor

instance Argument Int where
   makeArgDescr = flip defaultArgDescr 0
   
-- ArgDescr descr 0 parseExprM show termView arbitrary   