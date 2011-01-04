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
   ( -- * Generalized monadic views
     ViewM, match, build, newView, makeView, biArr, identity, (>>>)
   , canonical, canonicalWith
   , Control.Arrow.Arrow(..), Control.Arrow.ArrowChoice(..)
     -- * Simple views
   , View, ViewList, Match, belongsTo
   , simplify, simplifyWith, viewEquivalent, viewEquivalentWith
   , isCanonical, isCanonicalWith, matchM, canonicalM, viewList
     -- * Some combinators
   , swapView, listView, traverseView, associativeView
     -- * Properties on views
   , propIdempotence, propSoundness, propNormalForm
   ) where

import Common.Id
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Maybe
import Test.QuickCheck
import qualified Control.Category as C
import qualified Data.Traversable as T

----------------------------------------------------------------------------------
-- Generalized monadic view

data ViewM m a b where
   Prim    :: Id -> (a -> m b) -> (b -> a) -> ViewM m a b
   (:>>>:) :: ViewM m a b -> ViewM m b c -> ViewM m a c 
   First   :: ViewM m a b -> ViewM m (a, c) (b, c)
   Second  :: ViewM m b c -> ViewM m (a, b) (a, c)
   (:***:) :: ViewM m a c -> ViewM m b d -> ViewM m (a, b) (c, d)
   (:&&&:) :: ViewM m a b -> ViewM m a c -> ViewM m a (b, c)
   VLeft   :: ViewM m a b -> ViewM m (Either a c) (Either b c)
   VRight  :: ViewM m b c -> ViewM m (Either a b) (Either a c)
   (:+++:) :: ViewM m a c -> ViewM m b d -> ViewM m (Either a b) (Either c d)
   (:|||:) :: ViewM m a c -> ViewM m b c -> ViewM m (Either a b) c

instance Monad m => C.Category (ViewM m) where
   id    = identity
   v . w = w :>>>: v

instance Monad m => Arrow (ViewM m) where
   arr f  = Prim (newId "views.arr") (return . f) (error "Control.View.arr: function is not invertible")
   first  = First
   second = Second
   (***)  = (:***:)
   (&&&)  = (:&&&:)

instance Monad m => ArrowChoice (ViewM m) where
   left  = VLeft
   right = VRight
   (+++) = (:+++:)
   (|||) = (:|||:)

----------------------------------------------------------------------------------
-- Operations on a view

-- The preferred way of constructing a view
newView :: (IsId n, Monad m) => n -> (a -> m b) -> (b -> a) -> ViewM m a b
newView = Prim . newId

makeView :: Monad m => (a -> m b) -> (b -> a) -> ViewM m a b
makeView = newView "views.makeView"

biArr :: Monad m => (a -> b) -> (b -> a) -> ViewM m a b
biArr f = makeView (return . f)

match :: Monad m => ViewM m a b -> a -> m b
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

build :: ViewM m a b -> b -> a
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

canonical :: Monad m => ViewM m a b -> a -> m a
canonical = canonicalWith id

canonicalWith :: Monad m => (b -> b) -> ViewM m a b -> a -> m a
canonicalWith f view = liftM (build view . f) . match view

---------------------------------------------------------------
-- Simple views (based on a particular monad)

type View      = ViewM Maybe
type ViewList  = ViewM []
type Match a b = a -> Maybe b

simplify :: View a b -> a -> a
simplify = simplifyWith id

simplifyWith :: (b -> b) -> View a b -> a -> a
simplifyWith f view a = fromMaybe a (canonicalWith f view a)

belongsTo :: a -> View a b -> Bool
belongsTo a view = isJust (match view a)

viewEquivalent :: Eq b => View a b -> a -> a -> Bool
viewEquivalent = viewEquivalentWith (==)

viewEquivalentWith :: (b -> b -> Bool) -> View a b -> a -> a -> Bool
viewEquivalentWith eq view x y =
   case (match view x, match view y) of
      (Just a, Just b) -> a `eq` b
      _                -> False
      
isCanonical :: Eq a => View a b -> a -> Bool
isCanonical = isCanonicalWith (==)
      
isCanonicalWith :: (a -> a -> Bool) -> View a b -> a -> Bool
isCanonicalWith eq v a = maybe False (eq a) (canonical v a)

-- generalized match on a ViewM Maybe
matchM :: Monad m => View a b -> a -> m b
matchM v = maybe (Prelude.fail "no match") return . match v

-- generalized canonical element on a ViewM Maybe
canonicalM :: Monad m => View a b -> a -> m a
canonicalM v = maybe (Prelude.fail "no match") return . canonicalWith id v

viewList :: (Foldable m, Monad m) => ViewM m a b -> ViewList a b
viewList v = makeView (toList . match v) (build v)

---------------------------------------------------------------
-- Some combinators

identity :: Monad m => ViewM m a a 
identity = newView "views.identity" return id

swapView :: View (a, b) (b, a)
swapView = 
   let swap (a, b) = (b, a)
   in newView "views.swap" (return . swap) swap

-- | Specialized version of traverseView
listView :: Monad m => ViewM m a b -> ViewM m [a] [b]
listView = traverseView

traverseView :: (Monad m, T.Traversable f) => ViewM m a b -> ViewM m (f a) (f b)
traverseView v = makeView (T.mapM (match v)) (fmap (build v))
 
associativeView :: View a (a, a) -> ViewList a (a, a)
associativeView v = makeView (reverse . f) (build v)
 where f a = 
         case matchM v a of
           Just (x, y) -> [(x, y)] ++ [(x1, build v (x2, y)) | (x1, x2) <- f x]
                                   ++ [(build v (x, y1), y2) | (y1, y2) <- f y]
           Nothing -> []

---------------------------------------------------------------
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

{- proving a parameterized view equivalent to one with a "context"

abstr1 :: (a -> View b c) -> View (a, b) (a, c)
abstr1 fun = makeView f g
 where
   f (a, b) = do
      c <- match (fun a) b
      return (a, c)
   g (a, c) = (a, build (fun a) c)
   
abstr2 :: View (a, b) (a, c) -> (a -> View b c)
abstr2 v a = makeView f g
 where
   f b = do 
      (_, c) <- match v (a, b)
      return c
   g c = snd (build v (a, c)) -}