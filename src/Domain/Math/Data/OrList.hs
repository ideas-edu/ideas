{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Data.OrList
   ( OrList, OrSet, true, false, (<>)
   , isTrue, isFalse, fromBool, toOrList
   , noDuplicates, catOrList
   , oneDisjunct, orListView, orSetView
   ) where

import Common.Algebra.Boolean
import Common.Algebra.CoGroup
import Common.Algebra.Group
import Common.Classes
import Common.Rewriting
import Common.View
import Control.Applicative
import Control.Monad (liftM2)
import Data.Foldable (Foldable, foldMap, toList)
import Data.List
import Data.Traversable (Traversable, traverse)
import Domain.Logic.Formula (Logic((:||:)))
import Test.QuickCheck
import qualified Data.Set as S
import qualified Domain.Logic.Formula as Logic

instance Functor OrList where
   fmap f (OrList a) = OrList (fmap (map f) a)

instance Foldable OrList where
   foldMap f (OrList a) = maybe mempty (foldMap f) (fromWithZero a)

instance Traversable OrList where
   traverse f (OrList a) =
      maybe (pure mzero) (liftA toOrList . traverse f) (fromWithZero a)

------------------------------------------------------------
-- OrList data type

newtype OrList a = OrList (WithZero [a]) deriving
   (Eq, Ord, Monoid, MonoidZero, CoMonoid, CoMonoidZero)

instance BoolValue (OrList a) where
   fromBool b = if b then mzero else mempty
   isTrue  = isMonoidZero
   isFalse = isEmpty

instance Container OrList where
   singleton = OrList . pure . singleton
   getSingleton (OrList a) = fromWithZero a >>= getSingleton

instance IsTerm a => IsTerm (OrList a) where
   toTerm = toTerm . build orListView
   fromTerm expr = fromTerm expr >>= matchM orListView

instance Arbitrary a => Arbitrary (OrList a) where
   arbitrary = do
      n  <- choose (1, 3)
      xs <- vector n
      return (toOrList xs)

instance Show a => Show (OrList a) where
   show xs | isTrue  xs = "true"
           | isFalse xs = "false"
           | otherwise  = f xs
    where
      f = unwords . intersperse "or" . map show . toList

------------------------------------------------------------
-- Functions

-- | Remove duplicates
noDuplicates :: Eq a => OrList a -> OrList a
noDuplicates (OrList a) = OrList (fmap nub a)

oneDisjunct :: Monad m => (a -> m (OrList a)) -> OrList a -> m (OrList a)
oneDisjunct f (OrList a) =
   case fromWithZero a of
      Just [x] -> f x
      _ -> fail "oneDisjunct"

------------------------------------------------------------
-- OrSet data type

newtype OrSet a = OrSet (WithZero (S.Set a)) deriving
   (Eq, Ord, Monoid, MonoidZero, CoMonoid, CoMonoidZero)

instance (Show a, Ord a) => Show (OrSet a) where
   show = show . build orSetView

instance Ord a => BoolValue (OrSet a) where
   fromBool b = if b then mzero else mempty
   isTrue  = isMonoidZero
   isFalse = isEmpty

instance Container OrSet where
   singleton = OrSet . pure . singleton
   getSingleton (OrSet a) = fromWithZero a >>= getSingleton

------------------------------------------------------------
-- View to the logic data type

toOrList :: [a] -> OrList a
toOrList = mconcat . map singleton

orListView :: View (Logic a) (OrList a)
orListView = makeView f g
 where
   f p  = case p of
             Logic.Var a -> return (singleton a)
             Logic.T     -> return true
             Logic.F     -> return false
             a :||: b    -> liftM2 mappend (f a) (f b)
             _           -> Nothing
   g = fromOr . foldOrListWith (Or . Logic.Var)

orSetView :: Ord a => View (OrList a) (OrSet a)
orSetView = makeView (Just . f) g
 where
   f (OrList xs) = OrSet  (fmap S.fromList xs)
   g (OrSet  xs) = OrList (fmap S.toList xs)

foldOrList :: MonoidZero a => OrList a -> a
foldOrList xs
   | isTrue xs  = mzero
   | isFalse xs = mempty
   | otherwise  = foldr1 (<>) (toList xs)

foldOrListWith :: MonoidZero b => (a -> b) -> OrList a -> b
foldOrListWith f = foldOrList . fmap f

{-
foldOrListF :: (MonoidZero (f a), Container f) => OrList a -> f a
foldOrListF = foldOrListWith to -}

catOrList :: OrList (OrList a) -> OrList a
catOrList = foldOrList