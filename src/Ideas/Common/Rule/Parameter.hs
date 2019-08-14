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
-- This module defines transformations. Given a term, a transformation returns
-- a list of results (often a singleton list or the empty list). A
-- transformation can be parameterized with one or more Bindables.
-- Transformations rules can be lifted to work on more complex domains with
-- the LiftView type class.
--
-----------------------------------------------------------------------------

module Ideas.Common.Rule.Parameter
   ( -- * Reading inputs
     input, inputWith
   , transInput1, transInput2, transInput3, transInputWith
   , readRef2, readRef3
     -- * Writing outputs
   , output, outputWith
   , outputOnly, outputOnly2, outputOnly3, outputOnlyWith
   , writeRef2, writeRef3, writeRef2_, writeRef3_
     -- * Named parameters
   , ParamTrans
   , parameter1, parameter2, parameter3
   , transRef
   , supplyParameters
   ) where

import Control.Arrow
import Data.Maybe
import Ideas.Common.Context
import Ideas.Common.Environment
import Ideas.Common.Rule.Transformation
import Ideas.Common.View

----------------------------------------------------------------------------
-- Reading inputs

input :: Ref i -> Trans (i, a) b -> Trans a b
input = inputWith . readRef

inputWith :: Trans a i -> Trans (i, a) b -> Trans a b
inputWith f g = (f &&& identity) >>> g

transInput1 :: Ref i -> (i -> a -> Maybe b) -> Trans a b
transInput1 = transInputWith . readRef

transInput2 :: Ref i1 -> Ref i2 -> (i1 -> i2 -> a -> Maybe b) -> Trans a b
transInput2 r1 r2 = transInputWith (readRef2 r1 r2) . uncurry

transInput3 :: Ref i1 -> Ref i2 -> Ref i3 -> (i1 -> i2 -> i3 -> a -> Maybe b) -> Trans a b
transInput3 r1 r2 r3 = transInputWith (readRef3 r1 r2 r3) . uncurry3

transInputWith :: MakeTrans f => Trans a i -> (i -> a -> f b) -> Trans a b
transInputWith t = inputWith t . makeTrans . uncurry

readRef2 :: Ref a -> Ref b -> Trans x (a, b)
readRef2 r1 r2 = readRef r1 &&& readRef r2

readRef3 :: Ref a -> Ref b -> Ref c -> Trans x (a, b, c)
readRef3 r1 r2 r3 = readRef r1 &&& readRef2 r2 r3 >>^ to3

----------------------------------------------------------------------------
-- Writing outputs

output :: Ref o -> Trans a (b, o) -> Trans a b
output = outputWith . writeRef

outputWith :: Trans o x -> Trans a (b, o) -> Trans a b
outputWith f g = g >>> second f >>^ fst

outputOnly :: Ref o -> Trans a o -> Trans a a
outputOnly = outputOnlyWith . writeRef

outputOnly2 :: Ref o1 -> Ref o2 -> Trans a (o1, o2) -> Trans a a
outputOnly2 r1 = outputOnlyWith . writeRef2 r1

outputOnly3 :: Ref o1 -> Ref o2 -> Ref o3 -> Trans a (o1, o2, o3) -> Trans a a
outputOnly3 r1 r2 = outputOnlyWith . writeRef3 r1 r2

outputOnlyWith :: Trans o x -> Trans a o -> Trans a a
outputOnlyWith f g = ((g >>> f) &&& identity) >>^ snd

writeRef2 :: Ref a -> Ref b -> Trans (a, b) (a, b)
writeRef2 r1 r2 = writeRef r1 *** writeRef r2

writeRef2_ :: Ref a -> Ref b -> Trans (a, b) ()
writeRef2_ r1 r2 = writeRef2 r1 r2 >>^ const ()

writeRef3 :: Ref a -> Ref b -> Ref c -> Trans (a, b, c) (a, b, c)
writeRef3 r1 r2 r3 = from3 ^>> writeRef r1 *** writeRef2 r2 r3 >>^ to3

writeRef3_ :: Ref a -> Ref b -> Ref c -> Trans (a, b, c) ()
writeRef3_ r1 r2 r3 = writeRef3 r1 r2 r3 >>^ const ()

----------------------------------------------------------------------------
-- Named parameters

type ParamTrans i a = Trans (i, a) a

parameter1 :: Ref a -> (a -> b -> Maybe b) -> ParamTrans a b
parameter1 r1 f = first (transRef r1) >>> makeTrans (uncurry f)

parameter2 :: Ref a -> Ref b -> (a -> b -> c -> Maybe c) -> ParamTrans (a, b) c
parameter2 r1 r2 f = first (transRef r1 *** transRef r2) >>> makeTrans (uncurry (uncurry f))

parameter3 :: Ref a -> Ref b -> Ref c -> (a -> b -> c -> d -> Maybe d) -> ParamTrans (a, b, c) d
parameter3 r1 r2 r3 f = first (from3 ^>> t >>^ to3) >>> makeTrans (uncurry (\(a, b, c) -> f a b c))
 where
   t = transRef r1 *** (transRef r2 *** transRef r3)

transRef :: Ref a -> Trans a a
transRef r = (identity &&& readRefMaybe r) >>> uncurry fromMaybe ^>> writeRef r

supplyParameters :: ParamTrans b a -> Trans a b -> Transformation (Context a)
supplyParameters f g = transLiftContextIn $
   transUseEnvironment (g &&& identity) >>> first f

-----------------------------------------------------------------
-- helpers

from3 :: (a, b, c) -> (a, (b, c))
from3 (a, b, c) = (a, (b, c))

to3 :: (a, (b, c)) -> (a, b, c)
to3 (a, (b, c)) = (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c