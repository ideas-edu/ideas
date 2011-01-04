-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Type classes and instances.
--
-----------------------------------------------------------------------------
module Common.Classes 
   ( Apply(..), applicable, applyD, applyM
   , Switch(..), Zip(..)
   ) where

import Common.Utils (safeHead)
import Data.Maybe
import Control.Monad.Identity
import qualified Data.IntMap as IM 
import qualified Data.Map as M

-----------------------------------------------------------
-- * Type class |Apply|

-- | A type class for functors that can be applied to a value. Transformation, Rule, and
-- Strategy are all instances of this type class. Minimal complete definition: only one of
-- the two member functions should be defined.
class Apply t where
   apply    :: t a -> a -> Maybe a     -- ^ Returns zero or one results
   applyAll :: t a -> a -> [a]         -- ^ Returns zero or more results
   -- default definitions
   apply    ta = safeHead . applyAll ta
   applyAll ta = maybe [] return . apply ta

-- | Checks whether the functor is applicable (at least one result)
applicable :: Apply t => t a -> a -> Bool
applicable ta = isJust . apply ta

-- | If not applicable, return the current value (as default)
applyD :: Apply t => t a -> a -> a
applyD ta a = fromMaybe a (apply ta a)

-- | Same as apply, except that the result (at most one) is returned in some monad
applyM :: (Apply t, Monad m) => t a -> a -> m a
applyM ta = maybe (fail "applyM") return . apply ta

-----------------------------------------------------------
-- * Type class |Switch|

class Functor f => Switch f where
   switch :: Monad m => f (m a) -> m (f a)
         
instance Switch [] where
   switch = sequence

instance Switch Maybe where
   switch = maybe (return Nothing) (liftM Just)

instance Switch Identity where
   switch (Identity m) = liftM Identity m

instance Eq a => Switch (M.Map a) where
   switch m = do
      let (ns, ms) = unzip (M.toList m)
      as <- sequence ms 
      return $ M.fromAscList $ zip ns as

instance Switch IM.IntMap where
   switch m = do
      let (ns, ms) = unzip (IM.toList m)
      as <- sequence ms 
      return $ IM.fromAscList $ zip ns as

-----------------------------------------------------------
-- * Type class |Zip|
   
class Functor f => Zip f where
   fzip     :: f a -> f b -> f (a, b)
   fzipWith :: (a -> b -> c) -> f a -> f b -> f c
   -- default implementation
   fzip = fzipWith (,)
   fzipWith f a b = fmap (uncurry f) (fzip a b)

instance Zip [] where
   fzipWith = zipWith

instance Zip Maybe where
   fzipWith = liftM2