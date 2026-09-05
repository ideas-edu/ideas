{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
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
-- This module defines views on data-types, as described in "Canonical Forms
-- in Interactive Exercise Assistants"
--
-----------------------------------------------------------------------------

module Ideas.Common.View
   ( Control.Arrow.Arrow(..), Control.Arrow.ArrowChoice(..)
   , Control.Arrow.ArrowZero(..), Control.Arrow.ArrowPlus(..)
   , (>>>), (<<<)
     -- * @IsMatch@ type class
   , IsMatcher(..), belongsTo, viewEquivalent, viewEquivalentWith
   , Matcher, makeMatcher
     -- * @IsView@ type class
   , IsView(..), simplify, simplifyWith, simplifyWithM
   , canonical, canonicalWith, canonicalWithM, isCanonical, isCanonicalWith
     -- * Views
   , View, identity, makeView, matcherView
     -- * Isomorphisms
   , Isomorphism, from, to, inverse
     -- * Lifting with views
   , Lift(..), liftView, liftViewIn
     -- * Some combinators
   , swapView, listView, traverseView, ($<)
     -- * Packaging a view
   , ViewPackage(..)
     -- * Properties on views
   , propIdempotence, propSoundness, propNormalForm
   ) where

import Control.Arrow
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Id
import Test.QuickCheck
import qualified Control.Category as C
import qualified Data.Traversable as T

----------------------------------------------------------------------------------
-- @IsMatch@ type class

class IsMatcher f where
   match   :: f a b -> a -> Maybe b
   matcher :: f a b -> Matcher a b
   -- default definitions
   match   = runKleisli . unM . matcher
   matcher = makeMatcher . match

belongsTo :: IsMatcher f => a -> f a b -> Bool
belongsTo a view = isJust (match view a)

viewEquivalent :: (IsMatcher f, Eq b) => f a b -> a -> a -> Bool
viewEquivalent = viewEquivalentWith (==)

viewEquivalentWith :: IsMatcher f => (b -> b -> Bool) -> f a b -> a -> a -> Bool
viewEquivalentWith eq view x y =
   case (match view x, match view y) of
      (Just a, Just b) -> a `eq` b
      _                -> False

newtype Matcher a b = M { unM :: Kleisli Maybe a b }
   deriving (C.Category, Arrow, ArrowZero, ArrowPlus, ArrowChoice)

instance IsMatcher Matcher where
   matcher = id

makeMatcher :: (a -> Maybe b) -> Matcher a b
makeMatcher = M . Kleisli

----------------------------------------------------------------------------------
-- @IsView@ type class

-- |Minimal complete definition: @toView@ or both @match@ and @build@.
class IsMatcher f => IsView f where
   build  :: f a b -> b -> a
   toView :: f a b -> View a b
   -- default definitions
   build  f = build (toView f)
   toView f = makeView (match f) (build f)

canonical :: IsView f => f a b -> a -> Maybe a
canonical = canonicalWith id

canonicalWith :: IsView f => (b -> b) -> f a b -> a -> Maybe a
canonicalWith f = canonicalWithM (return . f)

canonicalWithM :: IsView f => (b -> Maybe b) -> f a b -> a -> Maybe a
canonicalWithM f view a =
   match view a >>= fmap (build view) . f

isCanonical :: (IsView f, Eq a) => f a b -> a -> Bool
isCanonical = isCanonicalWith (==)

isCanonicalWith :: IsView f => (a -> a -> Bool) -> f a b -> a -> Bool
isCanonicalWith eq v a = maybe False (eq a) (canonical v a)

simplify :: IsView f => f a b -> a -> a
simplify = simplifyWith id

simplifyWith :: IsView f => (b -> b) -> f a b -> a -> a
simplifyWith f = simplifyWithM (Just . f)

simplifyWithM :: IsView f => (b -> Maybe b) -> f a b -> a -> a
simplifyWithM f view a = fromMaybe a (canonicalWithM f view a)

----------------------------------------------------------------------------------
-- Views

data View a b where
   Prim     :: Matcher a b -> (b -> a) -> View a b
   (:@)     :: Id -> View a b -> View a b
   (:>>>:)  :: View a b -> View b c -> View a c
   (:***:)  :: View a c -> View b d -> View (a, b) (c, d)
   (:+++:)  :: View a c -> View b d -> View (Either a b) (Either c d)
   Traverse :: T.Traversable f => View a b -> View (f a) (f b)

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

instance IsMatcher View where
   matcher view =
      case view of
         Prim m _   -> m
         _ :@ v     -> matcher v
         v :>>>: w  -> matcher v >>> matcher w
         v :***: w  -> matcher v *** matcher w
         v :+++: w  -> matcher v +++ matcher w
         Traverse v -> makeMatcher $ T.mapM (match v)

instance IsView View where
   build view =
      case view of
         Prim _ f   -> f
         _ :@ v     -> build v
         v :>>>: w  -> build v <<< build w
         v :***: w  -> build v *** build w
         v :+++: w  -> biMap (build v) (build w)
         Traverse v -> fmap (build v)

   toView = id

instance HasId (View a b) where
   getId (n :@ _) = n
   getId _        = mempty
   changeId f (n :@ a) = f n :@ a
   changeId f a        = f mempty :@ a

instance Identify (View a b) where
   n @> v | a == mempty = v
          | otherwise   = a :@ v
    where
      a = newId n

makeView :: (a -> Maybe b) -> (b -> a) -> View a b
makeView = matcherView . makeMatcher

matcherView :: Matcher a b -> (b -> a) -> View a b
matcherView = Prim

identity :: C.Category f => f a a
identity = C.id

----------------------------------------------------------------------------------
-- Isomorphisms (embedding-projection pairs)

-- to ep . from ep == id
data Isomorphism a b = EP { pid :: Id, from :: a -> b, to :: b -> a }

instance C.Category Isomorphism where
   id    = id <-> id
   f . g = (from f . from g) <-> (to g . to f)

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

instance IsMatcher Isomorphism where
   match p = Just . from p

instance IsView Isomorphism where
   toView p = getId p @> makeView (match p) (to p)

instance HasId (Isomorphism a b) where
   getId = pid
   changeId f p = p { pid = f (pid p) }

instance Identify (Isomorphism a b) where
   (@>) = changeId . const . newId

inverse :: Isomorphism a b -> Isomorphism b a
inverse f = to f <-> from f

----------------------------------------------------------------------------------
-- Type class for lifting with Views

liftView :: Lift f => View a b -> f b -> f a
liftView v = liftWithM $ fmap (\b -> (b, build v)) . match v

liftViewIn :: Lift f => View a (b, c) -> f b -> f a
liftViewIn v = liftWithM $ fmap (\(b, c) -> (b, \x -> build v (x, c))) . match v

class Lift f where
   liftWith  :: (a -> (b, b -> a)) -> f b -> f a
   liftWithM :: (a -> Maybe (b, b -> a)) -> f b -> f a
   -- default
   liftWith f = liftWithM (Just . f)

----------------------------------------------------------------------------------
-- Some combinators

swapView :: Isomorphism (a, b) (b, a)
swapView = "views.swap" @> swap

-- | Specialized version of traverseView
listView :: View a b -> View [a] [b]
listView = traverseView

-- or is liftView a better name?
traverseView :: T.Traversable f => View a b -> View (f a) (f b)
traverseView = Traverse

($<) :: T.Traversable f => View a (f b) -> View b c -> View a (f c)
a $< b = a >>> traverseView b

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