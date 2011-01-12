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
   ( OrList
   , orList, true, false
   , isTrue, isFalse
   , disjunctions, idempotent, fromBool
   , oneDisjunct, orListView, orSetView
   ) where

import Common.Rewriting hiding (Monoid)
import Common.View
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable, foldMap)
import Data.List
import Data.Monoid
import Data.Traversable (Traversable, sequenceA)
import Domain.Logic.Formula (Logic((:||:)))
import Test.QuickCheck
import qualified Data.Set as S
import qualified Domain.Logic.Formula as Logic

------------------------------------------------------------
-- Data type

data OrList a = T | OrList [a] 
   deriving (Ord, Eq)

------------------------------------------------------------
-- Functions

orList :: [a] -> OrList a
orList = OrList

true, false :: OrList a
true  = T
false = OrList []

isTrue :: OrList a -> Bool
isTrue T = True
isTrue _ = False

isFalse :: OrList a -> Bool
isFalse (OrList []) = True
isFalse _           = False

disjunctions :: OrList a -> Maybe [a]
disjunctions T           = Nothing
disjunctions (OrList xs) = Just xs

-- | Remove duplicates
idempotent :: Eq a => OrList a -> OrList a
idempotent T           = T
idempotent (OrList xs) = OrList (nub xs)

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

instance Functor OrList where
   fmap _ T           = T
   fmap f (OrList xs) = OrList (map f xs)

instance Foldable OrList where
   foldMap _ T           = mempty
   foldMap f (OrList xs) = mconcat (map f xs)

instance Traversable OrList where
   sequenceA T           = pure T
   sequenceA (OrList xs) = OrList <$> sequenceA xs

instance Monoid (OrList a) where
   mempty      = false
   mappend p q = maybe T orList (liftM2 (++) (disjunctions p) (disjunctions q))

instance Monad OrList where
   return  = OrList . return
   m >>= f = foldMap id (fmap f m)

instance IsTerm a => IsTerm (OrList a) where
   toTerm = toTerm . build orListView
   fromTerm expr = fromTerm expr >>= matchM orListView

instance Arbitrary a => Arbitrary (OrList a) where
   arbitrary = do 
      n  <- choose (1, 3)
      xs <- vector n
      return (OrList xs)

instance CoArbitrary a => CoArbitrary (OrList a) where
   coarbitrary T           = variant (0 :: Int)
   coarbitrary (OrList xs) = variant (1 :: Int) . coarbitrary xs

instance Show a => Show (OrList a) where
   show T = "true"
   show (OrList xs) 
      | null xs   = "false"
      | otherwise = unwords (intersperse "or" (map show xs))

------------------------------------------------------------
-- View to the logic data type
 
orListView :: View (Logic a) (OrList a)
orListView = makeView f g 
 where
   f p  = case p of
             Logic.Var a -> return (return a)
             Logic.T     -> return true
             Logic.F     -> return false
             a :||: b    -> liftM2 mappend (f a) (f b)
             _           -> Nothing
   g xs = case disjunctions xs of
             Nothing -> Logic.T
             Just [] -> Logic.F
             Just ys -> foldr1 (:||:) (map Logic.Var ys)
             
-- Nothing in the codomain represents True
orSetView :: Ord a => View (OrList a) (Maybe (S.Set a))
orSetView = makeView f g 
 where
   f = return . fmap S.fromList . disjunctions
   g = maybe true (orList . S.toList)