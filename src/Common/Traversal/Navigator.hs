{-# LANGUAGE TypeFamilies #-}
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
module Common.Traversal.Navigator
   ( -- * Navigator type class
     Navigator(..), Location
   , isTop, isLeaf
   , hasLeft, hasRight, hasUp, hasDown
   , top, leftMost, rightMost, leftMostLeaf, rightMostLeaf
   , downTo, navigateTo, navigateTowards
     -- * Uniplate navigator
   , UniplateNavigator
   ) where

import Common.Traversal.Utils
import Common.Traversal.Iterator
import Common.Utils.Uniplate
import Control.Monad
import Data.Generics.Str
import Data.Maybe

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
   downLast = liftM (fixp right) . down
   downs    = maybe [] (fixpl right) . down
   arity    = length . downs

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
-- Uniplate navigator

data UniplateNavigator a = U [StrIterator a -> StrIterator a] (StrIterator a)

instance (Show a, Uniplate a ) => Show (UniplateNavigator a) where
   show a = show (current a) ++ " @ " ++ show (location a)

instance Uniplate a => Navigator (UniplateNavigator a) where
   up (U [] _)     = Nothing
   up (U (f:fs) a) = Just (U fs (f a))
  
   down     = downWith focusM
   downLast = downWith lastStrIterator
      
   left  (U fs a) = liftM (U fs) (previous a)
   right (U fs a) = liftM (U fs) (next a)
      
   location (U fs a) = reverse (rec a fs)
    where
      rec _ [] = []
      rec b (g:gs) = position b : rec (g b) gs

instance Update UniplateNavigator where
   update (U xs a) = (current a, U xs . flip replace a)

instance Uniplate a => Focus (UniplateNavigator a) where
   type Unfocus (UniplateNavigator a) = a
   focus   = U [] . focus . One
   unfocus = current . top

downWith :: Uniplate a => (Str a -> Maybe (StrIterator a)) 
                       -> UniplateNavigator a -> Maybe (UniplateNavigator a)
downWith make (U fs a) = liftM (U (f:fs)) (make cs)
 where
   (cs, g) = uniplate (current a)
   f = (`replace` a) . g . unfocus