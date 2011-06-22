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
   , View, identity, makeView
     -- * Isomorphisms
   , Isomorphism, from, to
     -- * Some combinators
   , swapView, listView, traverseView, (++>)
     -- * Packaging a view
   , ViewPackage(..)
     -- * Properties on views
   , propIdempotence, propSoundness, propNormalForm
   ) where

import Common.Id
import Control.Arrow
import Common.Classes
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
matchM v = maybe (fail "no match") return . match v

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
   Prim    :: (a -> Maybe b) -> (b -> a) -> View a b
   (:@)    :: Id -> View a b -> View a b
   (:>>>:) :: View a b -> View b c -> View a c 
   (:***:) :: View a c -> View b d -> View (a, b) (c, d)
   (:+++:) :: View a c -> View b d -> View (Either a b) (Either c d)

instance C.Category View where
   id    = makeView return id
   v . w = w :>>>: v

instance Arrow View where
   arr     = (!->)
   first   = (*** identity) 
   second  = (identity ***)
   (***)   = (:***:)
   f &&& g = copy >>> (f *** g)

instance BiArrow View where
   (<->) f = makeView (return . f)

instance ArrowChoice View where
   left    = (+++ identity)
   right   = (identity +++)
   (+++)   = (:+++:)
   f ||| g = (f +++ g) >>> merge

instance IsView View where
   match view =
      case view of
         Prim f _   -> f
         _ :@ v     -> match v
         v :>>>: w  -> \a      -> match v a >>= match w
         v :***: w  -> \(a, c) -> liftM2 (,) (match v a) (match w c)
         v :+++: w  -> either (liftM Left . match v) (liftM Right . match w)

   build view = 
      case view of
         Prim _ f   -> f
         _ :@ v     -> build v
         v :>>>: w  -> build v . build w
         v :***: w  -> build v *** build w
         v :+++: w  -> either (Left . build v) (Right . build w)

   toView = id

instance HasId (View a b) where
   getId (n :@ _) = n
   getId _        = mempty
   changeId f (n :@ a) = f n :@ a
   changeId f a        = f mempty :@ a

instance Identify (View a b) where
   n @> v | isEmptyId a = v
          | otherwise   = a :@ v
    where
      a = newId n

makeView :: (a -> Maybe b) -> (b -> a) -> View a b
makeView = Prim

identity :: C.Category f => f a a 
identity = C.id

----------------------------------------------------------------------------------
-- Isomorphisms (embedding-projection pairs)

-- to ep . from ep == id
data Isomorphism a b = EP { pid :: Id, from :: a -> b, to :: b -> a }

instance C.Category Isomorphism where
   id    = id <-> id
   f . g = from f . from g <-> to g . to f

instance Arrow Isomorphism where
   arr     = (!->)
   first   = (*** identity) 
   second  = (identity ***)
   p *** q = from p *** from q <-> to p *** to q
   f &&& g = copy >>> (f *** g)

instance BiArrow Isomorphism where
   (<->) = EP mempty

instance ArrowChoice Isomorphism where
   left    = (+++ identity)
   right   = (identity +++)
   p +++ q = from p +++ from q <-> to p +++ to q
   f ||| g = (f +++ g) >>> merge

instance IsView Isomorphism where
   toView p = getId p @> makeView (Just . from p) (to p)

instance HasId (Isomorphism a b) where 
   getId = pid
   changeId f p = p { pid = f (pid p) }

instance Identify (Isomorphism a b) where
   (@>) = changeId . const . newId

----------------------------------------------------------------------------------
-- Some combinators

swapView :: Isomorphism (a, b) (b, a)
swapView = "views.swap" @> swap

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

swap :: BiArrow arr => arr (a, b) (b, a)
swap = f <-> f
 where
   f :: (a, b) -> (b, a)
   f (a, b) = (b, a)

copy :: BiArrow arr => arr a (a, a)
copy = (\a -> (a, a)) <-> fst

merge :: BiArrow arr => arr (Either a a) a
merge = either id id <-> Left

----------------------------------------------------------------------------------
-- Packaging a view for documentation purposes

data ViewPackage where
   ViewPackage :: 
      (Show a, Show b, Eq a) => (String -> Maybe a) -> View a b -> ViewPackage

instance HasId ViewPackage where
   getId      (ViewPackage _ a) = getId a
   changeId f (ViewPackage p a) = ViewPackage p (changeId f a)

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