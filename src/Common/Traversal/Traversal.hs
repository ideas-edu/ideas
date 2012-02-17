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
module Common.Traversal.Traversal 
   ( Traversal, TraversalOver
   , -- * Iterations
     visitAll, visitOne, visitSome, visitMany
     -- * One-layer combinators
   , downAll, downOne, downSome
     -- * One-pass traversals
   , fulltd, spinetd, stoptd, oncetd, oncebu
     -- * Fixpoint traversals
   , innermost, outermost
   ) where

import Common.Traversal.Utils
import Common.Traversal.Iterator
import Common.Traversal.Navigator

type Traversal a = (a -> Maybe a) -> a -> Maybe a

type TraversalOver f a = (a -> Maybe a) -> f a -> Maybe (f a)

----------------------------------------------------------------------
-- Iterations

-- visit all elements
visitAll :: Iterator a => Traversal a
visitAll f = fmap (safe (next >=> visitAll f)) . f
      
-- visit exactly one element
visitOne :: Iterator a => Traversal a
visitOne f = f >|< (next >=> visitOne f)

-- visit at least one element
visitSome :: Iterator a => Traversal a
visitSome f = (f >=> visitMany f) >|< (next >=> visitSome f)

-- visit the possible elements 
visitMany :: Iterator a => Traversal a
visitMany f = Just . safe (next >=> visitMany f) . safe f

----------------------------------------------------------------------
-- One-layer combinators

downAll :: Navigator a => Traversal a
downAll = downWith visitAll

downOne :: Navigator a => Traversal a
downOne = downWith visitOne

downSome :: Navigator a => Traversal a
downSome = downWith visitSome

downWith :: Navigator a => Traversal (Horizontal a) -> Traversal a
downWith f = descend . unliftWrapper . f . liftHorizontal

----------------------------------------------------------------------
-- One-pass traversals

fulltd :: (Update f, Navigator (f a)) => TraversalOver f a
fulltd f = changeM f >=> (downAll (fulltd f) >|< leaf)

spinetd :: (Update f, Navigator (f a)) => TraversalOver f a
spinetd f = changeM f >=> (downOne (spinetd f) >|< leaf)

stoptd :: (Update f, Navigator (f a)) => TraversalOver f a
stoptd f = changeM f >|< downAll (stoptd f)

oncetd :: (Update f, Navigator (f a)) => TraversalOver f a
oncetd f = changeM f >|< downOne (oncetd f)

oncebu :: (Update f, Navigator (f a)) => TraversalOver f a
oncebu f = downOne (oncebu f) >|< changeM f

----------------------------------------------------------------------
-- fixpoint traversals

innermost :: (Update f, Navigator (f a)) => TraversalOver f a
innermost f = Just . fixp (oncebu f)

outermost :: (Update f, Navigator (f a)) => TraversalOver f a
outermost f = Just . fixp (oncetd f)

----------------------------------------------------------------------
-- Utility functions

liftHorizontal :: (a -> Maybe a) -> Horizontal a -> Maybe (Horizontal a)
liftHorizontal = liftWrapper

descend :: Navigator a => (a -> Maybe a) -> a -> Maybe a
descend f = down >=> f >=> up

leaf :: Navigator a => a -> Maybe a
leaf a = if isLeaf a then Just a else Nothing