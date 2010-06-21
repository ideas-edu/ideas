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
-- This module defines the type class Apply and some related utility functions.
--
-----------------------------------------------------------------------------
module Common.Apply where

import Common.Utils  (safeHead)
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
applyM ta = maybe (fail "applyM") return . apply ta