-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines the type class Apply and some related utility functions.
--
-----------------------------------------------------------------------------
module Common.Apply where

import Common.Utils  (safeHead)
import Control.Monad (join)
import Data.Maybe    (isJust, fromMaybe)

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
applyM ta a = maybe (fail "applyM") return (apply ta a)
 
-- | Apply a list of steps, and return at most one result
applyList :: Apply t => [t a] -> a -> Maybe a
applyList xs a = foldl (\ma t -> join $ fmap (apply t) ma) (Just a) xs

-- | Apply a list of steps, and return all results
applyListAll :: Apply t => [t a] -> a -> [a]
applyListAll xs a = foldl (\ma t -> concatMap (applyAll t) ma) [a] xs

-- | Apply a list of steps, and if there is no result, return the current value (as default)
applyListD :: Apply t => [t a] -> a -> a
applyListD xs a = foldl (\a t -> applyD t a) a xs

-- Apply a list of steps, and return the result (at most one) in some monad
applyListM :: (Apply t, Monad m) => [t a] -> a -> m a
applyListM xs a = foldl (\ma t -> ma >>= applyM t) (return a) xs