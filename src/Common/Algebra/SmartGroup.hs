{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
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
-----------------------------------------------------------------------------
module Common.Algebra.SmartGroup where

import Common.Algebra.Group
import Common.Algebra.CoGroup
import Control.Applicative
import Control.Monad (mplus)
import Data.Maybe

newtype Smart a = Smart {fromSmart :: a}
   deriving (Show, Eq, Ord, Functor, CoMonoid, MonoidZero, CoMonoidZero)

instance Applicative Smart where
   pure = Smart
   Smart f <*> Smart a = Smart (f a)

instance (CoMonoid a, Monoid a) => Monoid (Smart a) where
   mempty = Smart mempty
   mappend a b 
      | isEmpty a = b
      | isEmpty b = a
      | otherwise = liftA2 (<>) a b

--------------------------------------------------------------

newtype SmartZero a = SmartZero {fromSmartZero :: a}
   deriving (Show, Eq, Ord, Functor, MonoidZero, CoMonoid, CoMonoidZero)

instance Applicative SmartZero where
   pure = SmartZero
   SmartZero f <*> SmartZero a = SmartZero (f a)

instance (CoMonoidZero a, MonoidZero a) => Monoid (SmartZero a) where
   mempty = SmartZero mempty
   mappend a b 
      | isMonoidZero a || isMonoidZero b = mzero
      | otherwise = liftA2 (<>) a b

--------------------------------------------------------------

newtype SmartGroup a = SmartGroup {fromSmartGroup :: a}
   deriving (Show, Eq, Ord, Functor, CoMonoid, CoGroup, CoMonoidZero, MonoidZero)

instance Applicative SmartGroup where
   pure = SmartGroup
   SmartGroup f <*> SmartGroup a = SmartGroup (f a)

instance (CoGroup a, Group a) => Monoid (SmartGroup a) where
   mempty  = SmartGroup mempty
   mappend a b
      | isEmpty a = b
      | otherwise = fromMaybe (liftA2 (<>) a b) (matchGroup alg b)
    where
      alg = (a, \x y -> (a <> x) <> y, \x -> a <>- x, \x y -> (a <> x) <>- y)
   
instance (CoGroup a, Group a) => Group (SmartGroup a) where
   inverse a = fromMaybe (liftA inverse a) (matchGroup alg a)
    where
      alg = (mempty, \x y -> inverse x <>- y, id, \x y -> inverse x <> y)
   appendInv a b 
      | isEmpty a = inverse b
      | otherwise = fromMaybe (liftA2 (<>-) a b) (matchGroup alg b)
    where
      alg = (a, \x y -> (a <>- x) <>- y, \x -> a <> x, \x y -> (a <>- x) <> y)

--------------------------------------------------------------

type GroupMatch a b = (b, a -> a -> b, a -> b, a -> a -> b)

matchGroup :: CoGroup a => GroupMatch a b -> a -> Maybe b
matchGroup (emp, app, inv, appinv) a = 
   (if isEmpty a then Just emp else Nothing) `mplus`
   fmap (uncurry app) (isAppend a)  `mplus`
   fmap inv (isInverse a) `mplus`
   fmap (uncurry appinv) (isAppendInv a)