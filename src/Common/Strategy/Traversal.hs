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
module Common.Strategy.Traversal
   ( -- * Iterations
     visitAll, visitOne, visitSome, visitMany
     -- * One-layer combinators
   , downAll, downOne, downSome
     -- * One-pass traversals
   , fulltd, spinetd, stoptd, oncetd, oncebu, somewhere
   , once, onceWith, somewhereWith, topDown, bottomUp
     -- * Fixpoint traversals
   , innermost, outermost
   ) where

import Common.Traversal.Navigator
import Common.Strategy.Abstract
import Common.Strategy.Combinators hiding (not)
import Common.Rule
import Prelude hiding (repeat)

ruleUp :: Navigator a => Rule a  
ruleUp = minorRule "navigator.up" up

ruleDown :: Navigator a => Rule a  
ruleDown = minorRule "navigator.down" down

ruleRight :: Navigator a => Rule a  
ruleRight = minorRule "navigator.right" right

----------------------------------------------------------------------
-- Iterations

-- visit all elements
visitAll :: (IsStrategy f, Navigator a) => f a -> Strategy a
visitAll s = fix $ \a -> s <*> ((ruleRight <*> a) <|> check (not . hasRight))

-- visit exactly one element
visitOne :: (IsStrategy f, Navigator a) => f a -> Strategy a
visitOne s = fix $ \a -> s <|> (ruleRight <*> a)

-- visit at least one element
visitSome :: (IsStrategy f, Navigator a) => f a -> Strategy a
visitSome s = fix $ \a -> (s <*> try (ruleRight <*> visitMany s)) <|> (ruleRight <*> a)

-- visit the possible elements 
visitMany :: (IsStrategy f, Navigator a) => f a -> Strategy a
visitMany s = fix $ \a -> try s <*> ((ruleRight <*> a) <|> check (not . hasRight))

-- visit exactly one element
visitOneWith :: (IsStrategy f, Navigator a) => (a -> [Int]) -> f a -> Strategy a
visitOneWith p s = fix $ \a -> (check ok <*> s) <|> (ruleRight <*> a)
 where ok a = last (location a) `elem` maybe [] p (up a)

----------------------------------------------------------------------
-- One-layer combinators

downAll :: (IsStrategy f, Navigator a) => f a -> Strategy a
downAll = downWith visitAll

downOne :: (IsStrategy f, Navigator a) => f a -> Strategy a
downOne = downWith visitOne

downSome :: (IsStrategy f, Navigator a) => f a -> Strategy a
downSome = downWith visitSome

downWith :: (IsStrategy f, Navigator b) => (a -> f b) -> a -> Strategy b
downWith s = descend . s

----------------------------------------------------------------------
-- One-pass traversals

fulltd :: (IsStrategy f, Navigator a) => f a -> Strategy a
fulltd s = fix $ \a -> s <*> (downAll a <|> check isLeaf)

spinetd :: (IsStrategy f, Navigator a) => f a -> Strategy a
spinetd s = fix $ \a -> s <*> (downOne a <|> check isLeaf)

stoptd :: (IsStrategy f, Navigator a) => f a -> Strategy a
stoptd s = fix $ \a -> s |> downAll a

oncetd :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncetd s = fix $ \a -> s |> downOne a

oncebu :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncebu s = fix $ \a -> downOne a |> s

somewhere :: (IsStrategy f, Navigator a) => f a -> Strategy a
somewhere s = fix $ \a -> s <|> downOne a

----------------------------------------------------------------------
-- fixpoint traversals

innermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
innermost = repeat . oncebu

outermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
outermost = repeat . oncetd

----------------------------------------------------------------------
-- Utility functions

descend :: (IsStrategy f, Navigator a) => f a -> Strategy a
descend s = ruleDown <*> s <*> try ruleUp

somewhereWith :: (IsStrategy f, Navigator a) => String -> (a -> [Int]) -> f a -> Strategy a
somewhereWith _ p s = fix $ \a -> s <|> downWith (visitOneWith p) a

once :: (IsStrategy f, Navigator a) => f a -> Strategy a
once = downOne

onceWith :: (IsStrategy f, Navigator a) => String -> (a -> [Int]) -> f a -> Strategy a
onceWith _ = downWith . visitOneWith

topDown :: (IsStrategy f, Navigator a) => f a -> Strategy a
topDown = oncetd

bottomUp :: (IsStrategy f, Navigator a) => f a -> Strategy a
bottomUp = oncebu