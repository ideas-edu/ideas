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
-- Parameterized traversals based on the strategy language.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.Traversal
   ( -- * Parameterized traversals
     layer, traverse
     -- * Options
   , Option
   , topdown, bottomup, leftToRight, rightToLeft
   , full, spine, stop, once, leftmost, rightmost
   , traversalFilter, parentFilter
     -- * One-pass traversals
   , fulltd, fullbu, oncetd, oncebu, leftmostbu, leftmosttd
   , somewhere, somewhereWhen
   , oncetdPref, oncebuPref
     -- * Fixpoint traversals
   , innermost, outermost
     -- * Navigator rules
   , ruleUp, ruleDown, ruleDownLast, ruleLeft, ruleRight
   ) where

import Data.Monoid hiding ((<>))
import Ideas.Common.Classes
import Ideas.Common.Rule
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Combinators
import Ideas.Common.Traversal.Navigator
import Prelude hiding (repeat, not, traverse)
import qualified Data.Semigroup as Sem
import qualified Prelude

----------------------------------------------------------------------
-- One-layer combinators

data Visit = VisitFirst | VisitOne | VisitSome | VisitAll | VisitMany

visit :: (IsStrategy f, IsStrategy g) => Visit -> f a -> g a -> Strategy a
visit v next s = fix $ \a ->
   case v of
      VisitFirst -> s  |> next .*. a
      VisitOne   -> s .|. next .*. a
      VisitSome  -> s .*. try (next .*. visit VisitMany next s) .|. next .*. a
      VisitAll   -> s .*. (not next |> (next .*. a))
      VisitMany  -> try s .*. (not next |> (next .*. a))

----------------------------------------------------------------------
-- Parameterized traversals

layer :: (IsStrategy f, Navigator a) => [Option a] -> f a -> Strategy a
layer = layerWith . fromOptions

layerWith :: (IsStrategy f, Navigator a) => Info a -> f a -> Strategy a
layerWith tr s =
   goDown .*. findOk .*. visit (getVisit tr) (next .*. findOk) s .*. try ruleUp
 where
   (next, goDown)
      | getReversed tr = (ruleLeft, ruleDownLast)
      | otherwise      = (ruleRight, ruleDown)

   findOk =
      case getFilters tr of
         [] -> succeed
         ps -> fix $ \a -> check (\x -> all ($ x) ps) |> (next .*. a)

traverse :: (IsStrategy f, Navigator a) => [Option a] -> f a -> Strategy a
traverse = traverseWith . fromOptions

traverseWith :: (IsStrategy f, Navigator a) => Info a -> f a -> Strategy a
traverseWith tr s =
   fix $ \a ->
   case getOp tr of
      Sequence
         | getTopDown tr -> s .*. (descend a .|. check isLeaf)
         | otherwise     -> (descend a .|. check isLeaf) .*. s
      OrElse
         | getTopDown tr -> s |> descend a
         | otherwise     -> descend a |> s
      Prefer
         | getTopDown tr -> s ./. descend a
         | otherwise     -> descend a ./. s
      Choice             -> s .|. descend a
 where
   descend = layerWith tr

-----------------------------------------------------------------------

data Op = Sequence | OrElse | Choice | Prefer

data Info a = Info
   { getVisit    :: Visit
   , getOp       :: Op
   , getFilters  :: [a -> Bool]
   , getTopDown  :: Bool
   , getReversed :: Bool
   }

newtype Option a = O { unO :: Info a -> Info a }

instance Sem.Semigroup (Option a) where
   O f <> O g = O (f . g)

instance Monoid (Option a) where
   mempty  = O id
   mappend = (<>)

fromOptions :: [Option a] -> Info a
fromOptions xs = unO (mconcat xs) (Info VisitOne Choice [] True False)

topdown, bottomup :: Option a
topdown  = O $ \t -> t {getTopDown = True}
bottomup = O $ \t -> t {getTopDown = False}

leftToRight, rightToLeft :: Option a
leftToRight = O $ \t -> t {getReversed = False}
rightToLeft = O $ \t -> t {getReversed = True}

full, spine, stop, once :: Option a
full  = setOp Sequence `mappend` setVisit VisitAll
spine = setOp Sequence `mappend` setVisit VisitOne
stop  = setOp OrElse   `mappend` setVisit VisitAll
once  = setOp OrElse   `mappend` setVisit VisitOne

leftmost, rightmost :: Option a
leftmost  = leftToRight <> setOp OrElse
rightmost = rightToLeft <> setOp OrElse

setVisit :: Visit -> Option a
setVisit v = O $ \t -> t {getVisit = v}

setOp :: Op -> Option a
setOp c = O $ \t -> t {getOp = c}

traversalFilter :: (a -> Bool) -> Option a
traversalFilter ok = O $ \t -> t {getFilters = ok:getFilters t}

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

oncetdPref :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncetdPref = traverse [setOp Prefer, once, topdown]

oncebu :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncebu = traverse [once, bottomup]

oncebuPref :: (IsStrategy f, Navigator a) => f a -> Strategy a
oncebuPref = traverse [setOp Prefer, once, bottomup]

leftmostbu :: (IsStrategy f, Navigator a) => f a -> Strategy a
leftmostbu = traverse [setOp OrElse, setVisit VisitFirst, bottomup]

leftmosttd :: (IsStrategy f, Navigator a) => f a -> Strategy a
leftmosttd = traverse [setOp OrElse, setVisit VisitFirst, topdown]

somewhere :: (IsStrategy f, Navigator a) => f a -> Strategy a
somewhere = traverse []

-- as long as the predicate does not hold, go to the next layer
somewhereWhen :: (IsStrategy g, Navigator a) => (a -> Bool) -> g a -> Strategy a
somewhereWhen p s = fix $ \this ->
   check p .*. s .|. check (Prelude.not . p) .*. layer [] this

----------------------------------------------------------------------
-- fixpoint traverses

-- | left-most innermost traversal.
innermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
innermost = repeat . leftmostbu

-- | left-most outermost traversal.
outermost :: (IsStrategy f, Navigator a) => f a -> Strategy a
outermost = repeat . leftmosttd

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