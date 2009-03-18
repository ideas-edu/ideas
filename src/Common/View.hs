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
   ( Match, View, makeView, Simplification, makeSimplification
   , match, build, canonical, simplify
   , belongsTo, viewEquivalent, viewEquivalentWith
   , (>>>), Control.Arrow.Arrow(..), Control.Arrow.ArrowChoice(..)
   ) where

import Control.Arrow hiding ((>>>))
import qualified Control.Category as C
import Control.Monad
import Data.Maybe


-- For all v::View the following should hold:
--   1) simplify v a "is equivalent to" a
--   2) match (build b) equals Just b  
--         (but only for b that have at least one "a")
--
-- Derived property: simplification is idempotent

type Match a b = a -> Maybe b

data View a b = View 
   { match :: Match a b
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
-- Arrow combinators

identity :: View a a 
identity = undefined

(>>>) :: View a b -> View b c -> View a c
v >>> w = makeView (\a -> match v a >>= match w) (build v . build w)

instance C.Category View where
   id    = identity
   v . w = w >>> v

instance Arrow View where
   arr f = makeView 
      (return . f) 
      (error "Control.View.arr: function is not invertible")

   first v = makeView 
      (\(a, c) -> match v a >>= \b -> return (b, c)) 
      (\(b, c) -> (build v b, c))

   second v = makeView 
      (\(a, b) -> match v b >>= \c -> return (a, c)) 
      (\(a, c) -> (a, build v c))

   v *** w = makeView 
      (\(a, c) -> liftM2 (,) (match v a) (match w c)) 
      (\(b, d) -> (build v b, build w d))

   -- left-biased builder
   v &&& w = makeView 
      (\a -> liftM2 (,) (match v a) (match w a)) 
      (\(b, _) -> build v b)

instance ArrowChoice View where
   left v = makeView 
      (either (liftM Left . match v) (return . Right)) 
      (either (Left . build v) Right)

   right v = makeView 
      (either (return . Left) (liftM Right . match v)) 
      (either Left (Right . build v))

   v +++ w = makeView 
      (either (liftM Left . match v) (liftM Right . match w))  
      (either (Left . build v) (Right . build w))

   -- left-biased builder
   v ||| w = makeView 
      (either (match v) (match w))
      (Left . build v)