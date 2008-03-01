-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Apply where

import Common.Utils  (safeHead)
import Control.Monad (join)
import Data.Maybe    (isJust, fromMaybe)

class Apply t where
   apply    :: t a -> a -> Maybe a
   applyAll :: t a -> a -> [a] 
   -- default definitions
   apply    ta = safeHead . applyAll ta
   applyAll ta = maybe [] return . apply ta

applicable :: Apply t => t a -> a -> Bool
applicable ta = isJust . apply ta

applyD :: Apply t => t a -> a -> a
applyD ta a = fromMaybe a (apply ta a)

applyM :: (Apply t, Monad m) => t a -> a -> m a
applyM ta a = maybe (fail "applyM") return (apply ta a)
 
applyList :: Apply t => [t a] -> a -> Maybe a
applyList xs a = foldl (\ma t -> join $ fmap (apply t) ma) (Just a) xs

applyListAll :: Apply t => [t a] -> a -> [a]
applyListAll xs a = foldl (\ma t -> concatMap (applyAll t) ma) [a] xs

applyListD :: Apply t => [t a] -> a -> a
applyListD xs a = foldl (\a t -> applyD t a) a xs

applyListM :: (Apply t, Monad m) => [t a] -> a -> m a
applyListM xs a = foldl (\ma t -> ma >>= applyM t) (return a) xs