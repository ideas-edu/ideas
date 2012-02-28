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
   ( -- * One-layer combinators
     Visit(..), visit
   , visitAll, visitFirst, visitOne, visitSome, visitMany
     -- * One-pass traversals
   , fulltd, spinetd, stoptd, oncetd, oncebu, somewhere
   , once, onceWith, somewhereWith, topDown, bottomUp
     -- * Fixpoint traversals
   , innermost, outermost
   ) where

import Common.Traversal.Navigator hiding (Horizontal)
import Common.Strategy.Abstract
import Common.Strategy.Combinators hiding (multi)
import Common.Rule
import Prelude hiding (repeat, not)

----------------------------------------------------------------------
-- One-layer combinators

-- visit only first possible element
visitFirst :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
visitFirst next s = fix $ \a -> s |> (next <*> a)

-- visit exactly one element
visitOne :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
visitOne next s = fix $ \a -> s <|> (next <*> a)

-- visit all elements
visitAll :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
visitAll next s = fix $ \a -> s <*> (not next |> (next <*> a))

-- visit at least one element
visitSome :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
visitSome next s = fix $ \a -> 
   (s <*> try (next <*> visitMany next s)) <|> (next <*> a)

-- visit the possible elements 
visitMany :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
visitMany next s = fix $ \a -> try s <*> (not next |> (next <*> a))

data Visit = VisitFirst | VisitOne | VisitSome | VisitAll | VisitMany

visit :: (IsStrategy f, IsStrategy g) => Visit -> f a -> g a -> Strategy a
visit a =
   case a of
      VisitFirst -> visitFirst
      VisitOne   -> visitOne
      VisitSome  -> visitSome
      VisitAll   -> visitAll
      VisitMany  -> visitMany

----------------------------------------------------------------------
-- One-layer traversal

data OneLayer a = OneLayer Visit Bool [a -> Bool]

layer :: (IsStrategy f, Navigator a) => OneLayer a -> f a -> Strategy a
layer (OneLayer v rev ps) s = 
   goDown <*> visit v (next <*> findOk) (findOk <*> s) <*> try ruleUp
 where
   (next, goDown)
      | rev       = (ruleLeft, ruleDownLast)
      | otherwise = (ruleRight, ruleDown)
 
   findOk 
      | null ps   = succeed
      | otherwise = fix $ \a -> check (\x -> all ($ x) ps) |> (next <*> a)

----------------------------------------------------------------------
-- One-pass traversals

fulltd :: (IsStrategy f, Navigator a) => f a -> Strategy a
fulltd s = undefined -- fix $ \a -> s <*> (downAll a <|> check isLeaf)

spinetd :: (IsStrategy f, Navigator a) => f a -> Strategy a
spinetd s = undefined -- fix $ \a -> s <*> (downOne a <|> check isLeaf)

stoptd :: (IsStrategy f, Navigator a) => f a -> Strategy a
stoptd s = undefined -- fix $ \a -> s |> downAll a

oncetd :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncetd = traversal defaultTraversal {combinator = OrElse True}
-- s = fix $ \a -> s |> downOne a

oncebu :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncebu = traversal defaultTraversal {combinator = OrElse False}
-- s = fix $ \a -> downOne a |> s

somewhere :: (IsStrategy f, Navigator a) => f a -> Strategy a
somewhere = traversal defaultTraversal -- fix $ \a -> s <|> downOne a

----------------------------------------------------------------------
-- fixpoint traversals

innermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
innermost = repeat . oncebu

outermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
outermost = repeat . oncetd

----------------------------------------------------------------------
-- Utility functions



somewhereWith :: (IsStrategy f, Navigator a) => String -> (a -> [Int]) -> f a -> Strategy a
somewhereWith _ p = traversal $ parentFilter p defaultTraversal

once :: (IsStrategy f, Navigator a) => f a -> Strategy a
once = traversal defaultTraversal {combinator = Layer}

onceWith :: (IsStrategy f, Navigator a) => String -> (a -> [Int]) -> f a -> Strategy a
onceWith _ p = traversal $ parentFilter p defaultTraversal {combinator = Layer}

topDown :: (IsStrategy f, Navigator a) => f a -> Strategy a
topDown = oncetd

bottomUp :: (IsStrategy f, Navigator a) => f a -> Strategy a
bottomUp = oncebu

parentFilter :: Navigator a => (a -> [Int]) -> Traversal a -> Traversal a
parentFilter p t = t {oneLayer = f (oneLayer t)}
 where
   f (OneLayer v b ps) = OneLayer v b (ok:ps)
   ok a = last (location a) `elem` maybe [] p (up a)

-----------------------------------------------------------------------

data Traversal a = T
   { oneLayer   :: OneLayer a
   , combinator :: Combinator
   , multi      :: Bool
   }

data Combinator = Sequence Bool | OrElse Bool | Choice | Layer

defaultTraversal :: Traversal a
defaultTraversal = T (OneLayer VisitOne False []) Choice False

traversal :: (IsStrategy f, Navigator a) => Traversal a -> f a -> Strategy a
traversal tr s =
   (if multi tr then repeat else id) $
   case combinator tr of
      Sequence td 
         | td        -> fix $ \a -> s <*> (descend a <|> check isLeaf)
         | otherwise -> fix $ \a -> (descend a <|> check isLeaf) <*> s
      OrElse td 
         | td        -> fix $ \a -> s |> descend a
         | otherwise -> fix $ \a -> descend a |> s
      Choice         -> fix $ \a -> s <|> descend a
      Layer          -> descend s
 where
   descend a = layer (oneLayer tr) a

----------------------------------------------------------------------
-- Navigator rules

ruleUp :: Navigator a => Rule a  
ruleUp = minorRule "navigator.up" up

ruleDown :: Navigator a => Rule a  
ruleDown = minorRule "navigator.down" down

ruleDownLast :: Navigator a => Rule a  
ruleDownLast = minorRule "navigator.downlast" downLast

ruleLeft :: Navigator a => Rule a  
ruleLeft = minorRule "navigator.left" left

ruleRight :: Navigator a => Rule a  
ruleRight = minorRule "navigator.right" right