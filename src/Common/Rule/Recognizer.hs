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
-----------------------------------------------------------------------------
module Common.Rule.Recognizer
   ( -- * data type and type class
     Recognizable(..), Recognizer
     -- * Constructor functions
   , makeRecognizer, makeRecognizerBool, makeRecognizerList
   ) where

import Common.Environment
import Common.Id
import Common.View
import Data.Maybe
import Data.Monoid
               
-----------------------------------------------------------
--- Data type and type class

class Recognizable f where
   recognizer   :: f a -> Recognizer a
   recognizeAll :: f a -> a -> a -> [Environment]
   recognize    :: f a -> a -> a -> Maybe Environment
   -- default definitions
   recognizeAll    = unR . recognizer
   recognize r a b = listToMaybe $ recognizeAll r a b

newtype Recognizer a = R { unR :: a -> a -> [Environment] }

instance LiftView Recognizer where
   liftViewIn v r = R $ \a b -> do
      (x, _) <- matchM v a
      (y, _) <- matchM v b
      recognizeAll r x y

instance Monoid (Recognizer a) where
   mempty      = R $ \_ _ -> []
   mappend f g = R $ \x y -> recognizeAll f x y ++ recognizeAll g x y

instance Recognizable Recognizer where
   recognizer = id

-----------------------------------------------------------
--- Constructor functions

makeRecognizer :: (a -> a -> Maybe Environment) -> Recognizer a
makeRecognizer f = makeRecognizerList (\a b -> maybeToList $ f a b)

makeRecognizerBool :: (a -> a -> Bool) -> Recognizer a
makeRecognizerBool eq = makeRecognizerList $ \a b -> [ mempty | eq a b ]

makeRecognizerList :: (a -> a -> [Environment]) -> Recognizer a
makeRecognizerList = R