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
     ArgDescr, defaultArgument, parseArgument, showArgument
   , termViewArgument
   , defaultArgDescr, emptyArgDescr, simpleArgDescr
   , Argument(..), ArgValue(..), ArgValues, fromArgValue
   ) where

import Common.Id
import Common.Rewriting.Term
import Common.Utils
import Common.View
import Data.Typeable

-----------------------------------------------------------
--- Arguments

-- | A data type for describing an argument of a parameterized transformation
data ArgDescr a = ArgDescr
   { idArgument       :: Id                   -- ^ Identifier
   , defaultArgument  :: a                    -- ^ Current value
   , parseArgument    :: String -> Maybe a    -- ^ A parser
   , showArgument     :: a -> String          -- ^ A pretty-printer
   , termViewArgument :: Maybe (View Term a)  -- ^ Conversion to/from term
   }

instance HasId (ArgDescr a) where
   getId = idArgument
   changeId f d = d {idArgument = f (idArgument d)}

-- | An argument descriptor, paired with a value
data ArgValue = forall a . Typeable a => ArgValue (ArgDescr a)

fromArgValue :: Typeable a => ArgValue -> Maybe a
fromArgValue (ArgValue d) = cast (defaultArgument d)

-- | List of argument values
type ArgValues = [ArgValue]

instance Show ArgValue where
   show (ArgValue descr) = showId descr ++ "=" ++ showArgument descr (defaultArgument descr)

instance Eq ArgValue where
   ArgValue d1 == ArgValue d2 =
      case (termViewArgument d1, termViewArgument d2) of
         (Just f, Just g)   -> build f a1 == build g a2
         (Nothing, Nothing) -> showArgument d1 a1 == showArgument d2 a2
         _                  -> False
    where
      a1 = defaultArgument d1
      a2 = defaultArgument d2

-- | Constructor function for an argument descriptor that default type classes
defaultArgDescr :: (Show a, Read a, IsTerm a) => String -> a -> ArgDescr a
defaultArgDescr s a = ArgDescr (newId s) a readM show (Just termView)

emptyArgDescr :: String -> a -> (a -> String) -> ArgDescr a
emptyArgDescr s a f = 
   ArgDescr (newId s) a (const Nothing) f Nothing

simpleArgDescr :: (Show a, Read a) => String -> a -> ArgDescr a 
simpleArgDescr s a = (emptyArgDescr s a show) {parseArgument = readM}

-- | A type class for types which have an argument descriptor
class Typeable a => Argument a where
   makeArgDescr :: String -> ArgDescr a   -- ^ The first argument is the label of the argument descriptor

instance Argument Int where
   makeArgDescr = flip defaultArgDescr 0
   
instance Argument Term where
   makeArgDescr s = ArgDescr (newId s) (TNum 0) (const Nothing) show (Just termView)
   
instance Argument String where
   makeArgDescr s = ArgDescr (newId s) "" Just id Nothing