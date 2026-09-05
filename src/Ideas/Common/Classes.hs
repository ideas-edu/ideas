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
-- Type classes and instances.
--
-----------------------------------------------------------------------------

module Ideas.Common.Classes
   ( -- * Type class Apply
     Apply(applyAll), apply, applicable, applyD, applyList
     -- * Type class Container
   , Container(singleton, getSingleton)
     -- * Type class BiArrow
   , BiArrow(..)
     -- * Type class BiFunctor
   , BiFunctor(biMap, mapFirst, mapSecond), mapBoth
     -- * Type class Fix
   , Fix(..)
     -- * Boolean Algebra
   , BoolValue(..), Boolean(..)
   , ands, ors, implies, equivalent
     -- * Buggy and Minor properties
   , Buggy(..), Minor(..)
   ) where

import Control.Arrow
import Data.Maybe
import qualified Data.Set as S

-----------------------------------------------------------
-- Type class Apply

-- | A type class for functors that can be applied to a value. Transformation,
-- Rule, and Strategy are all instances of this type class.
class Apply t where
   applyAll :: t a -> a -> [a]  -- ^ Returns zero or more results

-- | Returns zero or one results
apply :: Apply t => t a -> a -> Maybe a
apply ta = listToMaybe . applyAll ta

-- | Checks whether the functor is applicable (at least one result)
applicable :: Apply t => t a -> a -> Bool
applicable ta = isJust . apply ta

-- | If not applicable, return the current value (as default)
applyD :: Apply t => t a -> a -> a
applyD ta a = fromMaybe a (apply ta a)

applyList :: Apply t => [t a] -> a -> Maybe a
applyList xs a = foldl (\m r -> m >>= apply r) (Just a) xs

-----------------------------------------------------------
-- Type class Container

-- | Instances should satisfy the following law: @getSingleton . singleton == Just@
class Container f where
   singleton    :: a   -> f a
   getSingleton :: f a -> Maybe a

instance Container [] where
   singleton        = return
   getSingleton [a] = Just a
   getSingleton _   = Nothing

instance Container S.Set where
   singleton    = S.singleton
   getSingleton = getSingleton . S.toList

-----------------------------------------------------------
-- Type class BiArrow

infix 1 <->

-- |Type class for bi-directional arrows. @<->@ should be used instead of
-- @arr@ from the arrow interface. Minimal complete definition: @<->@.
class Arrow arr => BiArrow arr where
   (<->) :: (a -> b) -> (b -> a) -> arr a b
   (!->) :: (a -> b) -> arr a b
   (<-!) :: (b -> a) -> arr a b
   -- default definitions
   (!->) f = f <-> errBiArrow
   (<-!) f = errBiArrow <-> f

errBiArrow :: a
errBiArrow = error "BiArrow: not bi-directional"

-----------------------------------------------------------
-- Type class BiFunctor

class BiFunctor f where
   biMap     :: (a -> c) -> (b -> d) -> f a b -> f c d
   mapFirst  :: (a -> b) -> f a c -> f b c
   mapSecond :: (b -> c) -> f a b -> f a c
   -- default definitions
   mapFirst  = flip biMap id
   mapSecond = biMap id

instance BiFunctor Either where
   biMap f g = either (Left . f) (Right . g)

instance BiFunctor (,) where
  biMap f g (a, b) = (f a, g b)

mapBoth :: BiFunctor f => (a -> b) -> f a a -> f b b
mapBoth f = biMap f f

-----------------------------------------------------------
-- Type class BiFunctor

class Fix a where
  fix :: (a -> a) -> a
  -- default implementation
  fix f = let a = f a in a

--------------------------------------------------------
-- Boolean algebra

-- Minimal complete definitions: (true/false, or fromBool) and isTrue/isFalse
class BoolValue a where
   true     :: a
   false    :: a
   fromBool :: Bool -> a
   isTrue   :: a -> Bool
   isFalse  :: a -> Bool
   -- default definitions
   true  = fromBool True
   false = fromBool False
   fromBool b = if b then true else false

class BoolValue a => Boolean a where
   (<&&>)     :: a -> a -> a
   (<||>)     :: a -> a -> a
   complement :: a -> a

instance BoolValue Bool where
   fromBool = id
   isTrue   = id
   isFalse  = not

instance BoolValue b => BoolValue (a -> b) where
   fromBool x = const (fromBool x)
   isTrue  = error "not implemented"
   isFalse = error "not implemented"

instance Boolean Bool where
   (<&&>)     = (&&)
   (<||>)     = (||)
   complement = not

instance Boolean b => Boolean (a -> b) where
   f <&&> g   = \x -> f x <&&> g x
   f <||> g   = \x -> f x <||> g x
   complement = (.) complement

ands :: Boolean a => [a] -> a -- or use mconcat with And monoid
ands xs | null xs   = true
        | otherwise = foldr1 (<&&>) xs

ors :: Boolean a => [a] -> a
ors xs | null xs   = false
       | otherwise = foldr1 (<||>) xs

implies :: Boolean a => a -> a -> a
implies a b = complement a <||> b

equivalent :: Boolean a => a -> a -> a
equivalent a b = (a <&&> b) <||> (complement a <&&> complement b)

-----------------------------------------------------------
-- Buggy and Minor properties

class Buggy a where
   buggy    :: a -> a
   setBuggy :: Bool -> a -> a
   isBuggy  :: a -> Bool
   -- default definition
   buggy = setBuggy True

class Minor a where
   minor    :: a -> a
   setMinor :: Bool -> a -> a
   isMinor  :: a -> Bool
   isMajor  :: a -> Bool
   -- default definition
   minor   = setMinor True
   isMajor = not . isMinor