{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines views on data-types, as described in "Canonical Forms
-- in Interactive Exercise Assistants"
--
-----------------------------------------------------------------------------
module Common.View 
   ( Control.Arrow.Arrow(..), Control.Arrow.ArrowChoice(..)
   , (>>>), (<<<)
     -- *  @IsView@ type class
   , IsView(..), matchM
   , canonical, canonicalWith, canonicalWithM, isCanonical, isCanonicalWith
   , simplify, simplifyWith
   , belongsTo, viewEquivalent, viewEquivalentWith
     -- * Views
   , View, identity, newView, makeView
     -- * Embedding-projection pairs
   , Projection, from, to, (<->)
     -- * Some combinators
   , swapView, listView, traverseView, (++>)
     -- * Properties on views
   , propIdempotence, propSoundness, propNormalForm
   ) where

import Common.Id
import Common.Utils (swap)
import Control.Arrow
import Control.Monad
import Data.Maybe
import Test.QuickCheck
import qualified Control.Category as C
import qualified Data.Traversable as T

----------------------------------------------------------------------------------
-- @IsView@ type class 

-- |Minimal complete definition: @toView@ or both @match@ and @build@.
class IsView f where
   match  :: f a b -> a -> Maybe b
   build  :: f a b -> b -> a
   toView :: f a b -> View a b
   -- default definitions
   match  f = match (toView f)
   build  f = build (toView f)
   toView f = makeView (match f) (build f)

-- |generalized monadic variant of @match@
matchM :: (Monad m, IsView f) => f a b -> a -> m b
matchM v = maybe (Prelude.fail "no match") return . match v

canonical :: IsView f => f a b -> a -> Maybe a
canonical = canonicalWith id

canonicalWith :: IsView f => (b -> b) -> f a b -> a -> Maybe a
canonicalWith f = canonicalWithM (return . f)

canonicalWithM :: IsView f => (b -> Maybe b) -> f a b -> a -> Maybe a
canonicalWithM f view a = 
   match view a >>= liftM (build view) . f

isCanonical :: (IsView f, Eq a) => f a b -> a -> Bool
isCanonical = isCanonicalWith (==)
      
isCanonicalWith :: IsView f => (a -> a -> Bool) -> f a b -> a -> Bool
isCanonicalWith eq v a = maybe False (eq a) (canonical v a)

simplify :: IsView f => f a b -> a -> a
simplify = simplifyWith id

simplifyWith :: IsView f => (b -> b) -> f a b -> a -> a
simplifyWith f view a = fromMaybe a (canonicalWith f view a)

belongsTo :: IsView f => a -> f a b -> Bool
belongsTo a view = isJust (match view a)

viewEquivalent :: (IsView f, Eq b) => f a b -> a -> a -> Bool
viewEquivalent = viewEquivalentWith (==)

viewEquivalentWith :: IsView f => (b -> b -> Bool) -> f a b -> a -> a -> Bool
viewEquivalentWith eq view x y =
   case (match view x, match view y) of
      (Just a, Just b) -> a `eq` b
      _                -> False

----------------------------------------------------------------------------------
-- Views

data View a b where
   Prim    :: Id -> (a -> Maybe b) -> (b -> a) -> View a b
   (:>>>:) :: View a b -> View b c -> View a c 
   First   :: View a b -> View (a, c) (b, c)
   Second  :: View b c -> View (a, b) (a, c)
   (:***:) :: View a c -> View b d -> View (a, b) (c, d)
   (:&&&:) :: View a b -> View a c -> View a (b, c)
   VLeft   :: View a b -> View (Either a c) (Either b c)
   VRight  :: View b c -> View (Either a b) (Either a c)
   (:+++:) :: View a c -> View b d -> View (Either a b) (Either c d)
   (:|||:) :: View a c -> View b c -> View (Either a b) c

instance C.Category View where
   id    = makeView return id
   v . w = w :>>>: v

instance Arrow View where
   arr f  = Prim (newId "views.arr") (return . f) (error "Control.View.arr: function is not invertible")
   first  = First
   second = Second
   (***)  = (:***:)
   (&&&)  = (:&&&:)

instance ArrowChoice View where
   left  = VLeft
   right = VRight
   (+++) = (:+++:)
   (|||) = (:|||:)

instance IsView View where
   match view =
      case view of
         Prim _ f _ -> f
         v :>>>: w  -> \a      -> match v a >>= match w
         First v    -> \(a, c) -> match v a >>= \b -> return (b, c)
         Second v   -> \(a, b) -> match v b >>= \c -> return (a, c)
         v :***: w  -> \(a, c) -> liftM2 (,) (match v a) (match w c)
         v :&&&: w  -> \a      -> liftM2 (,) (match v a) (match w a)
         VLeft v    -> either (liftM Left . match v) (return . Right)
         VRight v   -> either (return . Left) (liftM Right . match v)
         v :+++: w  -> either (liftM Left . match v) (liftM Right . match w)
         v :|||: w  -> either (match v) (match w)

   build view = 
      case view of
         Prim _ _ f -> f
         v :>>>: w  -> build v . build w
         First v    -> first (build v)
         Second v   -> second (build v)
         v :***: w  -> build v *** build w
         v :&&&: _  -> build v . fst -- left-biased
         VLeft v    -> either (Left . build v) Right
         VRight v   -> either Left (Right . build v)
         v :+++: w  -> either (Left . build v) (Right . build w)
         v :|||: _  -> Left . build v -- left-biased

identity :: C.Category f => f a a 
identity = C.id

-- The preferred way of constructing a view
newView :: IsId n => n -> (a -> Maybe b) -> (b -> a) -> View a b
newView = Prim . newId

makeView :: (a -> Maybe b) -> (b -> a) -> View a b
makeView = newView "views.makeView"

----------------------------------------------------------------------------------
-- Embedding-projection pairs

data Projection a b = EP { from :: a -> b, to :: b -> a}

instance C.Category Projection where
   id    = id <-> id
   f . g = from f . from g <-> to g . to f

instance Arrow Projection where
   arr f    = f <-> error "function is not invertible"
   first  p = first  (from p) <-> first  (to p)
   second p = second (from p) <-> second (to p)
   p *** q  = from p *** from q <-> to p *** to q
   p &&& q  = from p &&& from q <-> to p . fst -- left-biased

instance ArrowChoice Projection where
   left  p = left  (from p) <-> left  (to p)
   right p = right (from p) <-> right (to p)
   p +++ q = from p +++ from q <-> to p +++ to q
   p ||| q = from p ||| from q <-> Left . to p -- left-biased

instance IsView Projection where
   toView p = makeView (Just . from p) (to p)

infix 1 <->

(<->) :: (a -> b) -> (b -> a) -> Projection a b
(<->) = EP

----------------------------------------------------------------------------------
-- Some combinators

swapView :: View (a, b) (b, a)
swapView = newView "views.swap" (return . swap) swap

-- | Specialized version of traverseView
listView :: View a b -> View [a] [b]
listView = traverseView

-- or is liftView a better name?
traverseView :: T.Traversable f => View a b -> View (f a) (f b)
traverseView v = makeView (T.mapM (match v)) (fmap (build v))

infixr 2 ++>

(++>) :: View a b -> View a c -> View a (Either b c)
v1 ++> v2 = makeView f g
 where
   f a = liftM Left (match v1 a) `mplus` liftM Right (match v2 a)
   g   = either (build v1) (build v2)

----------------------------------------------------------------------------------
-- Properties on views

propIdempotence :: (Show a, Eq a) => Gen a -> View a b -> Property
propIdempotence g v = forAll g $ \a -> 
   let b = simplify v a
   in b == simplify v b

propSoundness :: Show a => (a -> a -> Bool) -> Gen a -> View a c -> Property
propSoundness semEq g v = forAll g $ \a -> 
   let b = simplify v a
   in semEq a b
   
propNormalForm :: (Show a, Eq a) => Gen a -> View a b -> Property
propNormalForm g v = forAll g $ \a -> a == simplify v a