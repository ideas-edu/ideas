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
-----------------------------------------------------------------------------
module Domain.Math.Data.OrList 
   ( OrList
   , orList, (\/), true, false
   , isTrue, isFalse
   , disjunctions, normalize, idempotent
   ) where

import Control.Monad
import Common.Traversable
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

------------------------------------------------------------
-- Instances

-- local helper
joinOr :: OrList (OrList a) -> OrList a
joinOr = maybe T (foldr (\/) false) . disjunctions

instance Functor OrList where
   fmap _ T           = T
   fmap f (OrList xs) = OrList (map f xs)

instance Monad OrList where
   return  = OrList . return
   m >>= f = joinOr (fmap f m)

instance Once OrList where
   onceM = useOnceJoin

instance Switch OrList where
   switch T           = return T
   switch (OrList xs) = liftM orList (sequence xs)

instance Crush OrList where
   crush T           = []
   crush (OrList xs) = xs

instance OnceJoin OrList where
   onceJoinM _ T = mzero
   onceJoinM f (OrList xs) = rec xs
    where
      rec []     = mzero
      rec (x:xs) = liftM (\/ orList xs) (f x) `mplus`
                   liftM (return x \/) (rec xs)

instance Arbitrary a => Arbitrary (OrList a) where
   arbitrary = do 
      n  <- choose (1, 3)
      xs <- vector n
      return (OrList xs)
   coarbitrary T           = variant 0
   coarbitrary (OrList xs) = variant 1 . coarbitrary xs

instance Show a => Show (OrList a) where
   show T = "true"
   show (OrList xs) 
      | null xs   = "false"
      | otherwise = unwords (intersperse "or" (map show xs))