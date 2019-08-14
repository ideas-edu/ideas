{-# LANGUAGE TypeFamilies #-}
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

module Ideas.Common.Traversal.Iterator
   ( -- * Iterator type class
     Iterator(..), isFirst, isFinal, hasNext, hasPrevious
   , searchForward, searchBackward, searchNext, searchPrevious, searchWith
     -- * List iterator
   , ListIterator
   ) where

import Control.Monad
import Data.List
import Data.Maybe
import Ideas.Common.Traversal.Utils
import Test.QuickCheck

---------------------------------------------------------------
-- Iterator type class

class Iterator a where
   next     :: a -> Maybe a
   previous :: a -> Maybe a
   first    :: a -> a
   final    :: a -> a
   position :: a -> Int
   -- default implementations
   first    = fixp previous
   final    = fixp next
   position = pred . length . fixpl previous

instance Iterator a => Iterator (Mirror a) where
   next     = liftWrapper previous
   previous = liftWrapper next
   first    = mapWrapper  final
   final    = mapWrapper  first

isFirst :: Iterator a => a -> Bool
isFirst = not . hasPrevious

isFinal :: Iterator a => a -> Bool
isFinal = not . hasNext

hasNext :: Iterator a => a -> Bool
hasNext = isJust . next

hasPrevious :: Iterator a => a -> Bool
hasPrevious = isJust . previous

searchForward :: Iterator a => (a -> Bool) -> a -> Maybe a
searchForward = searchWith next

searchBackward :: Iterator a => (a -> Bool) -> a -> Maybe a
searchBackward = searchWith previous

searchNext :: Iterator a => (a -> Bool) -> a -> Maybe a
searchNext p = next >=> searchForward p

searchPrevious :: Iterator a => (a -> Bool) -> a -> Maybe a
searchPrevious p = previous >=> searchBackward p

searchWith :: (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
searchWith f p = rec
 where
   rec a | p a       = Just a
         | otherwise = f a >>= rec

---------------------------------------------------------------
-- List iterator

data ListIterator a = LI [a] a [a]
   deriving Eq

instance Show a => Show (ListIterator a) where
   show (LI xs y ys) =
      let listLike   = brackets . intercalate ","
          brackets s = "[" ++ s ++ "]"
          focusOn  s = "<<" ++ s ++ ">>"
      in listLike (map show (reverse xs) ++ focusOn (show y) : map show ys)

instance Iterator (ListIterator a) where
   previous (LI (x:xs) y ys) = Just (LI xs x (y:ys))
   previous _                = Nothing

   next     (LI xs x (y:ys)) = Just (LI (x:xs) y ys)
   next     _                = Nothing

   position (LI xs _ _) = length xs

instance Focus (ListIterator a) where
   type Unfocus (ListIterator a) = [a]

   focusM []     = Nothing
   focusM (x:xs) = Just (LI [] x xs)

   unfocus (LI xs y ys) = reverse xs ++ y : ys

instance Update ListIterator where
   update (LI xs a ys) = (a, \b -> LI xs b ys)

instance Arbitrary a => Arbitrary (ListIterator a) where
   arbitrary = liftM3 LI arbitrary arbitrary arbitrary