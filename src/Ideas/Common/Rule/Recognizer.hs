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
-----------------------------------------------------------------------------

module Ideas.Common.Rule.Recognizer
   ( -- * data type and type class
     Recognizable(..), Recognizer
     -- * Constructor functions
   , makeRecognizer, makeRecognizerTrans
   ) where

import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Common.Environment
import Ideas.Common.Rule.Transformation
import Ideas.Common.View

-----------------------------------------------------------
--- Data type and type class

class Recognizable f where
   recognizer     :: f a -> Recognizer a
   recognizeAll   :: f a -> a -> a -> [Environment]
   recognize      :: f a -> a -> a -> Maybe Environment
   recognizeTrans :: f a -> Trans (a, a) ()
   -- default definitions
   recognizeAll r a b = map snd $ transApply (recognizeTrans r) (a, b)
   recognize    r a b = listToMaybe $ recognizeAll r a b
   recognizeTrans     = unR . recognizer

newtype Recognizer a = R { unR :: Trans (a, a) () }

instance Lift Recognizer where
   liftWithM f r =
      let t = makeTrans (fmap fst . f)
      in R (t *** t >>> unR r)

instance Sem.Semigroup (Recognizer a) where
   f <> g = R $ unR f `mappend` unR g

instance Monoid (Recognizer a) where
   mempty  = R mempty
   mappend = (<>)

instance Recognizable Recognizer where
   recognizer = id

instance HasRefs (Recognizer a) where
   allRefs = allRefs . unR

-----------------------------------------------------------
--- Constructor functions

makeRecognizer :: (a -> a -> Bool) -> Recognizer a
makeRecognizer eq = makeRecognizerTrans $ transMaybe $ \(x, y) ->
   if eq x y then Just () else Nothing

makeRecognizerTrans :: Trans (a, a) () -> Recognizer a
makeRecognizerTrans = R