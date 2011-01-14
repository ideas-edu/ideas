{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
   , isTrue, isFalse
   , disjunctions, idempotent, fromBool
   , oneDisjunct, orListView, orSetView
   ) where

import Common.Classes
import Common.Rewriting hiding (Monoid)
import Common.View
import Control.Monad
import Data.Foldable (Foldable)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Domain.Logic.Formula (Logic((:||:)))
import Test.QuickCheck
import qualified Data.Set as S
import qualified Domain.Logic.Formula as Logic

------------------------------------------------------------
-- Data type

newtype OrList a = OrList (ListMonoid a)
   deriving (Eq, Ord, Monoid, Functor, Foldable, Traversable)

instance Collection OrList where
   singleton = OrList . singleton
   fromList  = OrList . fromList

------------------------------------------------------------
-- Functions

true, false :: OrList a
true  = OrList absorbing
false = mempty

isTrue :: OrList a -> Bool
isTrue (OrList a) = isAbsorbing a

isFalse :: OrList a -> Bool
isFalse = maybe False null . disjunctions

disjunctions :: OrList a -> Maybe [a]
disjunctions xs
   | isTrue xs = Nothing
   | otherwise = Just (toList xs)

-- | Remove duplicates
idempotent :: Eq a => OrList a -> OrList a
idempotent = maybe true (fromList . nub) . disjunctions

oneDisjunct :: Monad m => (a -> m (OrList a)) -> OrList a -> m (OrList a)
oneDisjunct f xs = 
   case disjunctions xs of 
      Just [a] -> f a
      _ -> fail "oneDisjunct"

fromBool :: Bool -> OrList a
fromBool b = if b then true else false

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
   g xs = case disjunctions xs of
             Nothing -> Logic.T
             Just [] -> Logic.F
             Just ys -> foldr1 (:||:) (map Logic.Var ys)
             
-- True results in a failed match
orSetView :: Ord a => View (OrList a) (S.Set a)
orSetView = makeView f g 
 where
   f = fmap S.fromList . disjunctions
   g = fromList . S.toList
   
------------------------------------------------------------
-- ListMonoid

class Monoid a => AbsMonoid a where
   absorbing   :: a          -- the absorbing element of a monoid operation
   isAbsorbing :: a -> Bool  -- is it the absorbing element?

newtype ListMonoid a = LM (Absorbing [a])
   deriving (Eq, Ord, Monoid, Functor, Foldable, Traversable, AbsMonoid)
   -- Functor and Traversable are not available for set

instance Collection ListMonoid where
   singleton = fromList . return
   fromList  = LM . A . Just

newtype Absorbing a = A { unA :: Maybe a }
   deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Monoid a => Monoid (Absorbing a) where
   mempty = A (Just mempty)
   mappend x y = A (liftM2 mappend (unA x) (unA y))

instance Monoid a => AbsMonoid (Absorbing a) where
   absorbing   = A Nothing
   isAbsorbing = isNothing . unA
   
------------------------------------------------------------
-- SetMonoid

newtype SetMonoid a = SM (Absorbing (S.Set a))
   deriving (Eq, Ord, Monoid, Foldable, AbsMonoid)

instance Collection SetMonoid where
   singleton = SM . A . Just . singleton