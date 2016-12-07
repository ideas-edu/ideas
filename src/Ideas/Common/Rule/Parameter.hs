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
  -- , TransReader(..), TransWriter(..)
   ) where

import Control.Arrow
import Data.Maybe
import Ideas.Common.Context
import Ideas.Common.Environment
import Ideas.Common.Rule.Transformation
import Ideas.Common.View

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
supplyParameters f g = inputWith (transMaybe g) f

supplyContextParameters :: ParamTrans b a -> Trans a b -> Transformation (Context a)
supplyContextParameters f g = transLiftContextIn $
   transUseEnvironment (g &&& identity) >>> first f

transRef :: Ref a -> Trans a a
transRef r = (identity &&& readRefMaybe r) >>> arr (uncurry fromMaybe) >>> writeRef r

parameter1 :: Ref a -> ParamTrans a b -> ParamTrans a b
parameter1 r1 f = first (transRef r1) >>> f

parameter2 :: Ref a -> Ref b -> ParamTrans (a, b) c -> ParamTrans (a, b) c
parameter2 r1 r2 f = first (transRef r1 *** transRef r2) >>> f

parameter3 :: Ref a -> Ref b -> Ref c -> ParamTrans (a, b, c) d -> ParamTrans (a, b, c) d
parameter3 r1 r2 r3 f = first (arr from3 >>> t >>> arr to3) >>> f
 where
   t = transRef r1 *** (transRef r2 *** transRef r3)

---------------------------------------------------------------------------

inputWith :: Trans c i -> Trans (i, c) d -> Trans c d
inputWith f g = (f &&& identity) >>> g

outputWith :: Trans b c -> Trans a b -> Trans a c
outputWith f g = f <<< g

readRef2 :: Ref a -> Ref b -> Trans x (a, b)
readRef2 r1 r2 = readRef r1 &&& readRef r2

readRef3 :: Ref a -> Ref b -> Ref c -> Trans x (a, b, c)
readRef3 r1 r2 r3 = readRef r1 &&& readRef2 r2 r3 >>> arr to3

writeRef2 :: Ref a -> Ref b -> Trans (a, b) ()
writeRef2 r1 r2 = (writeRef_ r1 *** writeRef_ r2) >>> arr fst

writeRef3 :: Ref a -> Ref b -> Ref c -> Trans (a, b, c) ()
writeRef3 r1 r2 r3 = arr from3 >>> (writeRef_ r1 *** writeRef2 r2 r3) >>> arr fst

checkTrans :: Trans a x -> Trans a a
checkTrans f = (f &&& identity) >>> arr snd
{-
class TransReader a where
   transReader :: Trans x a
   
instance (TransReader a, TransReader b) => TransReader (a, b) where
   transReader = transReader &&& transReader

instance (TransReader a, TransReader b, TransReader c) => TransReader (a, b, c) where
   transReader = transReader &&& transReader &&& transReader >>> arr to3
      
class TransWriter a where
   transWriter :: Trans a a
   
instance (TransWriter a, TransWriter b) => TransWriter (a, b) where
   transWriter = transWriter *** transWriter
   
instance (TransWriter a, TransWriter b, TransWriter c) => TransWriter (a, b, c) where
   transWriter = arr from3 >>> transWriter *** transWriter *** transWriter >>> arr to3 -}
    
from3 :: (a, b, c) -> (a, (b, c))
from3 (a, b, c) = (a, (b, c))

to3 :: (a, (b, c)) -> (a, b, c)
to3 (a, (b, c)) = (a, b, c)