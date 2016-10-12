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
-- This module defines transformations. Given a term, a transformation returns
-- a list of results (often a singleton list or the empty list). A
-- transformation can be parameterized with one or more Bindables.
-- Transformations rules can be lifted to work on more complex domains with
-- the LiftView type class.
--
-----------------------------------------------------------------------------

module Ideas.Common.Rule.Parameter
   ( ParamTrans
   , supplyParameters, supplyContextParameters
   , parameter1, parameter2, parameter3
   , transLookup1, transLookup2, transLookup3
   ) where

import Control.Arrow
import Ideas.Common.Context
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rule.EnvironmentMonad
import Ideas.Common.Rule.Transformation
import Ideas.Common.View
import Data.Typeable

-----------------------------------------------------------
--- Bindables

type ParamTrans a b = Trans (a, b) b

supplyParameters :: ParamTrans b a -> (a -> Maybe b) -> Transformation a
supplyParameters f g = transMaybe g &&& identity >>> f

transLookup1 :: Typeable a => Ref a -> (a -> Transformation b) -> Transformation b
transLookup1 r1 f = lookupRef r1 (parameter1Ref r1 f)

transLookup2 :: (Typeable a, Typeable b) => Ref a -> Ref b -> (a -> b -> Transformation c) -> Transformation c
transLookup2 r1 r2 f = transLookup1 r1 $ transLookup1 r2 . f

transLookup3 :: (Typeable a, Typeable b, Typeable c) => Ref a -> Ref b -> Ref c -> (a -> b -> c -> Transformation d) -> Transformation d
transLookup3 r1 r2 r3 f = transLookup1 r1 $ transLookup2 r2 r3 . f

lookupRef :: Typeable b => Ref b -> ParamTrans b a -> Transformation a
lookupRef r f = ((transGetEnvironment >>> transMaybe (r ?)) &&& identity) >>> f

supplyContextParameters :: ParamTrans b a -> (a -> EnvMonad b) -> Transformation (Context a)
supplyContextParameters f g = transLiftContextIn $
   transUseEnvironment (transEnvMonad g &&& identity) >>> first f

parameter1Ref :: Typeable a => Ref a -> (a -> Transformation b) -> ParamTrans a b
parameter1Ref r f = first (transRef r >>> arr f) >>> app

parameter1 :: (IsId n1, Reference a) => n1 -> (a -> Transformation b) -> ParamTrans a b
parameter1 = parameter1Ref . makeRef

parameter2 :: (IsId n1, IsId n2, Reference a, Reference b)
           => n1 -> n2 -> (a -> b -> Transformation c) -> ParamTrans (a, b) c
parameter2 n1 n2 f = first (bindValue n1 *** bindValue n2 >>> arr (uncurry f)) >>> app

parameter3 :: (IsId n1, IsId n2, IsId n3, Reference a, Reference b, Reference c)
           => n1 -> n2 -> n3 -> (a -> b -> c -> Transformation d) -> ParamTrans (a, b, c) d
parameter3 n1 n2 n3 f = first ((\(a, b, c) -> (a, (b, c))) ^>>
   bindValue n1 *** (bindValue n2 *** bindValue n3) >>^
   (\(a, (b, c)) -> f a b c))
           >>> app

bindValue :: (IsId n, Reference a) => n -> Trans a a
bindValue = transRef . makeRef