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
   , orList, (\/), true, false
   , isTrue, isFalse
   , disjunctions, normalize, idempotent, fromBool
   , oneDisjunct, orListView
   ) where

import Common.View
import Control.Monad
import Common.Classes
import Common.Rewriting
import qualified Domain.Logic.Formula as Logic
import Domain.Logic.Formula (Logic((:||:)))
import Test.QuickCheck
import Data.List (intersperse, nub, sort)

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
isTrue T = Prelude.True
isTrue _ = False

isFalse :: OrList a -> Bool
isFalse (OrList []) = True
isFalse _           = False

disjunctions :: OrList a -> Maybe [a]
disjunctions T           = Nothing
disjunctions (OrList xs) = Just xs

(\/) :: OrList a -> OrList a -> OrList a
p \/ q = maybe T orList (liftM2 (++) (disjunctions p) (disjunctions q))

-- | Sort the propositions and remove duplicates
normalize :: Ord a => OrList a -> OrList a
normalize T           = T
normalize (OrList xs) = OrList (nub $ sort xs)

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

-- local helper
joinOr :: OrList (OrList a) -> OrList a
joinOr = maybe T (foldr (\/) false) . disjunctions

instance Rewrite a => Rewrite (OrList a)

instance Functor OrList where
   fmap _ T           = T
   fmap f (OrList xs) = OrList (map f xs)

instance Monad OrList where
   return  = OrList . return
   m >>= f = joinOr (fmap f m)

instance Switch OrList where
   switch T           = return T
   switch (OrList xs) = liftM orList (sequence xs)

instance Crush OrList where
   crush T           = []
   crush (OrList xs) = xs

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
             a :||: b    -> liftM2 (\/) (f a) (f b)
             _           -> Nothing
   g xs = case disjunctions xs of
             Nothing -> Logic.T
             Just [] -> Logic.F
             Just ys -> foldr1 (:||:) (map Logic.Var ys)