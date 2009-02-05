-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines views on data-types
--
-----------------------------------------------------------------------------
module Common.View 
   ( View, makeView, Simplification, makeSimplification
   , match, build, canonical, simplify
   , belongsTo, viewEquivalent, viewEquivalentWith
   , (>>>)
   ) where

import Control.Monad
import Data.Maybe


-- For all v::View the following should hold:
--   1) simplify v a "is equivalent to" a
--   2) match (build b) equals Just b  
--         (but only for b that have at least one "a")
--
-- Derived property: simplification is idempotent

data View a b = View 
   { match :: a -> Maybe b
   , build :: b -> a
   }

type Simplification a = View a a

makeView :: (a -> Maybe b) -> (b -> a) -> View a b
makeView = View

makeSimplification :: (a -> a) -> Simplification a
makeSimplification f = makeView (return . f) id

canonical :: View a b -> a -> Maybe a
canonical view = liftM (build view) . match view

simplify :: View a b -> a -> a
simplify view a = fromMaybe a (canonical view a)

---------------------------------------------------------------

belongsTo :: a -> View a b -> Bool
belongsTo a view = isJust (match view a)

viewEquivalent :: Eq b => View a b -> a -> a -> Bool
viewEquivalent = viewEquivalentWith (==)

viewEquivalentWith :: (b -> b -> Bool) -> View a b -> a -> a -> Bool
viewEquivalentWith eq view x y =
   case (match view x, match view y) of
      (Just a, Just b) -> a `eq` b
      _                -> False
      
---------------------------------------------------------------

(>>>) :: View a b -> View b c -> View a c
v1 >>> v2 = makeView (\a -> match v1 a >>= match v2) (build v1 . build v2)