{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, 
       DeriveTraversable #-}
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
module Domain.Math.Data.OrList 
   ( OrList, true, false
   , isTrue, isFalse, fromBool
   , noDuplicates, catOrList
   , oneDisjunct, orListView, orSetView
   ) where

import Common.Algebra.Group hiding (idempotent)
import Common.Algebra.Boolean
import Common.Classes
import Common.Rewriting
import Common.View
import Control.Monad
import Data.Foldable (Foldable, toList)
import Control.Applicative
import Data.List
import Data.Traversable (Traversable)
import Domain.Logic.Formula (Logic((:||:)))
import Test.QuickCheck
import qualified Data.Set as S
import qualified Domain.Logic.Formula as Logic

------------------------------------------------------------
-- Data type

newtype OrList a = OrList (WithZero [a])
   deriving (Eq, Ord, Monoid, MonoidZero, Functor, Foldable, Traversable)

instance BoolValue (OrList a) where
   fromBool b = if b then zero else mempty

instance Collection OrList where
   singleton = OrList . pure . singleton
   fromList  = OrList . pure . fromList

------------------------------------------------------------
-- Functions

isTrue :: OrList a -> Bool
isTrue (OrList a) = isZero a

isFalse :: OrList a -> Bool
isFalse (OrList a) = maybe False null (fromWithZero a)

-- | Remove duplicates
noDuplicates :: Eq a => OrList a -> OrList a
noDuplicates (OrList a) = OrList (fmap nub a)

oneDisjunct :: Monad m => (a -> m (OrList a)) -> OrList a -> m (OrList a)
oneDisjunct f (OrList a) = 
   case fromWithZero a of 
      Just [x] -> f x
      _ -> fail "oneDisjunct"

------------------------------------------------------------
-- Instances

instance Rewrite a => Rewrite (OrList a)

instance IsTerm a => IsTerm (OrList a) where
   toTerm = toTerm . build orListView
   fromTerm expr = fromTerm expr >>= matchM orListView

instance Arbitrary a => Arbitrary (OrList a) where
   arbitrary = do 
      n  <- choose (1, 3)
      xs <- vector n
      return (fromList xs)

instance Show a => Show (OrList a) where
   show xs | isTrue  xs = "true"
           | isFalse xs = "false"
           | otherwise  = f xs
    where
      f = unwords . intersperse "or" . map show . toList

------------------------------------------------------------
-- View to the logic data type
 
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
             
-- True results in a failed match
orSetView :: Ord a => View (OrList a) (S.Set a)
orSetView = makeView f g 
 where
   f = fromWithZero . foldOrListWith (pure . singleton)
   g = fromList . S.toList
   
foldOrList :: MonoidZero a => OrList a -> a
foldOrList xs
   | isTrue xs  = zero
   | isFalse xs = mempty
   | otherwise  = foldr1 (<>) (toList xs)
   
foldOrListWith :: MonoidZero b => (a -> b) -> OrList a -> b
foldOrListWith f = foldOrList . fmap f

{-
foldOrListF :: (MonoidZero (f a), Collection f) => OrList a -> f a
foldOrListF = foldOrListWith singleton -}

catOrList :: OrList (OrList a) -> OrList a
catOrList = foldOrList