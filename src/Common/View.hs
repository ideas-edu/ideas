-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
   ( -- * Generalized monadic views
     ViewM, match, build, makeView, biArr, identity, (>>>)
   , canonical, canonicalWith
   , Control.Arrow.Arrow(..), Control.Arrow.ArrowChoice(..)
     -- * Simple views
   , View, ViewList, Match, belongsTo
   , simplify, simplifyWith, viewEquivalent, viewEquivalentWith
   , isCanonical, isCanonicalWith, matchM, canonicalM, viewList
     -- * Some combinators
   , listView, switchView, ( #> ), associativeView
     -- * Properties on views
   , propIdempotence, propSoundness, propNormalForm
   ) where

import Common.Traversable
import Control.Arrow hiding ((>>>))
import Control.Monad
import Data.Maybe
import Test.QuickCheck
import qualified Control.Category as C

----------------------------------------------------------------------------------
-- Generalized monadic view

-- For all v::View the following should hold:
--   1) simplify v a "is equivalent to" a
--   2) match (build b) equals Just b  
--         (but only for b that have at least one "a")
--
-- Derived property: simplification is idempotent

data ViewM m a b = ViewM
   { match :: a -> m b
   , build :: b -> a
   }

makeView :: Monad m => (a -> m b) -> (b -> a) -> ViewM m a b
makeView = ViewM

biArr :: Monad m => (a -> b) -> (b -> a) -> ViewM m a b
biArr f g = makeView (return . f) g

canonical :: Monad m => ViewM m a b -> a -> m a
canonical = canonicalWith id

canonicalWith :: Monad m => (b -> b) -> ViewM m a b -> a -> m a
canonicalWith f view = liftM (build view . f) . match view

---------------------------------------------------------------
-- Arrow combinators

identity :: Monad m => ViewM m a a 
identity = makeView return id

(>>>) :: Monad m => ViewM m a b -> ViewM m b c -> ViewM m a c
v >>> w = makeView (\a -> match v a >>= match w) (build v . build w)

instance Monad m => C.Category (ViewM m) where
   id    = identity
   v . w = w >>> v
   
instance Monad m => Arrow (ViewM m) where
   arr f = biArr f (error "Control.View.arr: function is not invertible")

   first v = makeView 
      (\(a, c) -> match v a >>= \b -> return (b, c)) 
      (first (build v))

   second v = makeView 
      (\(a, b) -> match v b >>= \c -> return (a, c)) 
      (second (build v))

   v *** w = makeView 
      (\(a, c) -> liftM2 (,) (match v a) (match w c)) 
      (build v *** build w)

   -- left-biased builder
   v &&& w = makeView 
      (\a -> liftM2 (,) (match v a) (match w a)) 
      (\(b, _) -> build v b)

instance Monad m => ArrowChoice (ViewM m) where
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

viewList :: Crush m => ViewM m a b -> ViewList a b
viewList v = makeView (crush . match v) (build v)

---------------------------------------------------------------
-- Some combinators

listView :: Monad m => ViewM m a b -> ViewM m [a] [b]
listView v = makeView (mapM (match v)) (map (build v))

switchView :: (Monad m, Switch f) => ViewM m a b -> ViewM m (f a) (f b)
switchView v = makeView (switch . fmap (match v)) (fmap (build v))

( #> ) :: MonadPlus m => (a -> Bool) -> ViewM m a b -> ViewM m a b
p #> v = makeView f (build v)
 where f a = guard (p a) >> match v a
 
associativeView :: View a (a,a) -> ViewList a (a,a)
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