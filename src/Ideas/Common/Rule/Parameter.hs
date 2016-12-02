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
   , transInput1, transInput2, transInput3
   , parameter1, parameter2, parameter3
     -----
   , inputWith, readRef, readRef2, readRef3
   , outputWith, writeRef, writeRef2, writeRef3
   , checkTrans
   ) where

import Control.Arrow
import Ideas.Common.Context
import Ideas.Common.Environment
import Ideas.Common.Rule.Transformation
import Ideas.Common.View
import Data.Typeable

transInput1 :: MakeTrans f => Ref x -> (x -> a -> f b) -> Trans a b
transInput1 r1 f = inputWith (readRef r1) $ makeTrans $ \(x, p) -> f x p

transInput2 :: MakeTrans f => Ref x -> Ref y -> (x -> y -> a -> f b) -> Trans a b
transInput2 r1 r2 f = inputWith (readRef2 r1 r2) $ makeTrans $ \((x, y), p) -> f x y p

transInput3 :: MakeTrans f => Ref x -> Ref y -> Ref z -> (x -> y -> z -> a -> f b) -> Trans a b
transInput3 r1 r2 r3 f = inputWith (readRef3 r1 r2 r3) $ makeTrans $ \((x, y, z), p) -> f x y z p

-----------------------------------------------------------
--- Bindables

type ParamTrans a b = Trans (a, b) b

supplyParameters :: ParamTrans b a -> (a -> Maybe b) -> Transformation a
supplyParameters f g = transMaybe g &&& identity >>> f

supplyContextParameters :: ParamTrans b a -> Trans a b -> Transformation (Context a)
supplyContextParameters f g = transLiftContextIn $
   transUseEnvironment (g &&& identity) >>> first f

parameter1 :: Typeable a => Ref a -> ParamTrans a b -> ParamTrans a b
parameter1 r1 f = first (transRef r1) >>> f

parameter2 :: (Typeable a, Typeable b) => Ref a -> Ref b -> ParamTrans (a, b) c -> ParamTrans (a, b) c
parameter2 r1 r2 f = first (transRef r1 *** transRef r2) >>> f

parameter3 :: (Typeable a, Typeable b, Typeable c) => Ref a -> Ref b -> Ref c -> ParamTrans (a, b, c) d -> ParamTrans (a, b, c) d
parameter3 r1 r2 r3 f = first (arr from3 >>> t >>> arr to3) >>> f
 where
   from3 (x, y, z) = (x, (y, z))
   to3 (x, (y, z)) = (x, y, z)
   t = transRef r1 *** (transRef r2 *** transRef r3)

---------------------------------------------------------------------------

inputWith :: Trans c i -> Trans (i, c) d -> Trans c d
inputWith f g = (f &&& identity) >>> g

outputWith :: Trans b c -> Trans a b -> Trans a c
outputWith f g = f <<< g

readRef :: Ref a -> Trans x a
readRef r = transReadRefM r >>> transMaybe id

readRef2 :: Ref a -> Ref b -> Trans x (a, b)
readRef2 r1 r2 = readRef r1 &&& readRef r2

readRef3 :: Ref a -> Ref b -> Ref c -> Trans x (a, b, c)
readRef3 r1 r2 r3 = (readRef2 r1 r2) &&& readRef r3 >>> arr f
 where
   f ((a, b), c) = (a, b, c)

writeRef :: Typeable a => Ref a -> Trans a ()
writeRef r = arr Just >>> transWriteRefM r

writeRef2 :: (Typeable a, Typeable b) => Ref a -> Ref b -> Trans (a, b) ()
writeRef2 r1 r2 = (writeRef r1 *** writeRef r2) >>> arr fst

writeRef3 :: (Typeable a, Typeable b, Typeable c) => Ref a -> Ref b -> Ref c -> Trans (a, b, c) ()
writeRef3 r1 r2 r3 = arr f >>> (writeRef2 r1 r2 *** writeRef r3) >>> arr fst
 where
   f (a, b, c) = ((a, b), c)

checkTrans :: Trans a x -> Trans a a
checkTrans f = (f &&& identity) >>> arr snd
