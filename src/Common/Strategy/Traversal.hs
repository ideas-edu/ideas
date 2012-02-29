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
   ( layer, traverse, Option
     -- * Options
   , topdown, bottomup, leftToRight, rightToLeft
   , full, spine, stop, once, parentFilter
     -- * One-pass traversals
   , fulltd, fullbu, oncetd, oncebu, somewhere
     -- * Fixpoint traversals
   , innermost, outermost
   ) where

import Common.Traversal.Navigator
import Common.Strategy.Abstract
import Common.Strategy.Combinators
import Common.Rule
import Data.Monoid
import Prelude hiding (repeat, not)

----------------------------------------------------------------------
-- One-layer combinators

data Visit = VisitFirst | VisitOne | VisitSome | VisitAll | VisitMany

visit :: (IsStrategy f, IsStrategy g) => Visit -> f a -> g a -> Strategy a
visit v next s = fix $ \a -> 
   case v of
      VisitFirst -> s  |> next <*> a
      VisitOne   -> s <|> next <*> a
      VisitSome  -> s <*> try (next <*> visit VisitMany next s) <|> next <*> a
      VisitAll   -> s <*> (not next |> (next <*> a))
      VisitMany  -> try s <*> (not next |> (next <*> a))

----------------------------------------------------------------------
-- Parameterized traversals

layer :: (IsStrategy f, Navigator a) => [Option a] -> f a -> Strategy a
layer = layerWith . fromOptions

layerWith :: (IsStrategy f, Navigator a) => Info a -> f a -> Strategy a
layerWith tr s = 
   goDown <*> findOk <*> visit (getVisit tr) (next <*> findOk) s <*> try ruleUp
 where
   (next, goDown)
      | getReversed tr = (ruleLeft, ruleDownLast)
      | otherwise      = (ruleRight, ruleDown)
 
   findOk =
      case getFilters tr of
         [] -> succeed
         ps -> fix $ \a -> check (\x -> all ($ x) ps) |> (next <*> a)

traverse :: (IsStrategy f, Navigator a) => [Option a] -> f a -> Strategy a
traverse = traverseWith . fromOptions

traverseWith :: (IsStrategy f, Navigator a) => Info a -> f a -> Strategy a
traverseWith tr s =
   fix $ \a -> 
   case getCombinator tr of
      Sequence 
         | getTopDown tr -> s <*> (descend a <|> check isLeaf)
         | otherwise     -> (descend a <|> check isLeaf) <*> s
      OrElse 
         | getTopDown tr -> s |> descend a
         | otherwise     -> descend a |> s
      Choice             -> s <|> descend a
 where
   descend = layerWith tr

-----------------------------------------------------------------------

data Combinator = Sequence | OrElse | Choice

data Info a = Info
   { getVisit      :: Visit
   , getCombinator :: Combinator
   , getFilters    :: [a -> Bool]
   , getTopDown    :: Bool
   , getReversed   :: Bool
   }

newtype Option a = O { unO :: Info a -> Info a }

instance Monoid (Option a) where
   mempty            = O id
   O f `mappend` O g = O (f . g) 

fromOptions :: [Option a] -> Info a
fromOptions xs = unO (mconcat xs) (Info VisitOne Choice [] True False)

topdown, bottomup :: Option a
topdown  = O $ \t -> t {getTopDown = True}
bottomup = O $ \t -> t {getTopDown = False}

leftToRight, rightToLeft :: Option a
leftToRight = O $ \t -> t {getReversed = False}
rightToLeft = O $ \t -> t {getReversed = True}

full, spine, stop, once :: Option a
full  = setCombinator Sequence `mappend` setVisit VisitAll
spine = setCombinator Sequence `mappend` setVisit VisitOne
stop  = setCombinator OrElse   `mappend` setVisit VisitAll
once  = setCombinator OrElse   `mappend` setVisit VisitOne

setVisit :: Visit -> Option a
setVisit v = O $ \t -> t {getVisit = v}

setCombinator :: Combinator -> Option a
setCombinator c = O $ \t -> t {getCombinator = c}

parentFilter :: Navigator a => (a -> [Int]) -> Option a
parentFilter p = O $ \t -> t {getFilters = ok:getFilters t}
 where
   ok a = maybe True (\x -> childnr a `elem` p x) (up a)

----------------------------------------------------------------------
-- One-pass traverses

fulltd :: (IsStrategy f, Navigator a) => f a -> Strategy a
fulltd = traverse [full, topdown]

fullbu :: (IsStrategy f, Navigator a) => f a -> Strategy a
fullbu = traverse [full, bottomup]

oncetd :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncetd = traverse [once, topdown]

oncebu :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncebu = traverse [once, bottomup]

somewhere :: (IsStrategy f, Navigator a) => f a -> Strategy a
somewhere = traverse []

----------------------------------------------------------------------
-- fixpoint traverses

innermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
innermost = repeat . oncebu

outermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
outermost = repeat . oncetd

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