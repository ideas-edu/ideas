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
-- This module defines type classes for iteration and navigation
--
-----------------------------------------------------------------------------
module Common.Navigator
   ( -- * Iterator type class
     Iterator(..), hasNext, hasPrevious
     -- * Navigator type class
   , Navigator(..), Location
   , isTop, isLeaf
   , hasLeft, hasRight, hasUp, hasDown
   , top, leftMost, rightMost, leftMostLeaf, rightMostLeaf
     -- navigation
   , downTo, navigateTo, navigateTowards
   ) where

import Control.Monad
import Data.Maybe

---------------------------------------------------------------
-- Iterator type class

class Iterator a where
   next     :: a -> Maybe a
   previous :: a -> Maybe a
   first    :: a -> a
   final    :: a -> a
   -- default implementations
   first = fixp previous
   final = fixp next
   
hasNext :: Iterator a => a -> Bool
hasNext = isJust . next

hasPrevious :: Iterator a => a -> Bool
hasPrevious = isJust . previous

---------------------------------------------------------------
-- Navigator type class

type Location = [Int]

-- | For a minimal complete definition, provide an implemention for downs or
-- allDowns. All other functions need an implementation as well, except for
-- change. Note that a constructor (a -> f a) is not included in the type class
-- to allow additional type class constraints on type a.
class Navigator a where
   -- navigation
   up       :: a -> Maybe a
   down     :: a -> Maybe a
   downLast :: a -> Maybe a
   left     :: a -> Maybe a
   right    :: a -> Maybe a
   -- extra navigation
   downs    :: a -> [a]
   -- inspection
   location :: a -> Location
   arity    :: a -> Int
   -- default definitions
   down       = downTo 0
   downLast a = downTo (arity a - 1) a
   arity      = length . downs
   left a     = join $ liftM2 downTo (liftM pred (lastLoc a)) (up a)
   right a    = join $ liftM2 downTo (liftM succ (lastLoc a)) (up a)

isTop, isLeaf :: Navigator a => a -> Bool
isTop  = not . hasUp
isLeaf = not . hasDown

hasLeft, hasRight, hasUp, hasDown :: Navigator a => a -> Bool
hasLeft  = isJust . left
hasRight = isJust . right
hasUp    = isJust . up
hasDown  = isJust . down

top, leftMost, rightMost :: Navigator a => a -> a
top       = fixp up
leftMost  = fixp left
rightMost = fixp right

leftMostLeaf, rightMostLeaf :: Navigator a => a -> a
leftMostLeaf  = fixp down
rightMostLeaf = fixp downLast

downTo :: Navigator a => Int -> a -> Maybe a
downTo n
   | n < 0 = const Nothing
   | True  = listToMaybe . drop n . downs
   
navigateTo :: Navigator a => Location -> a -> Maybe a
navigateTo is a = go (navigation (location a) is) a 
 where 
   go = foldr (>=>) Just 

navigateTowards :: Navigator a => Location -> a -> a
navigateTowards is a = go (navigation (location a) is) a 
 where 
   go = foldr (\f g -> safe (fmap g . f)) id

navigation :: Navigator a => Location -> Location -> [a -> Maybe a]
navigation old new = replicate upnr up ++ map downTo ds
 where
   same = length (takeWhile id (zipWith (==) old new))
   upnr = length old - same
   ds   = drop same new

---------------------------------------------------------------
-- helpers

safe :: (a -> Maybe a) -> a -> a 
safe f a = fromMaybe a (f a)

fixp :: (a -> Maybe a) -> a -> a
fixp f = last . fixpl f

fixpl :: (a -> Maybe a) -> a -> [a]
fixpl f a = a : maybe [] (fixpl f) (f a)

lastLoc :: Navigator a => a -> Maybe Int
lastLoc = listToMaybe . reverse . location