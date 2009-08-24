module Domain.Math.Data.OrList where

import Control.Monad
import Common.Traversable
import Test.QuickCheck
import Data.List (intersperse)

------------------------------------------------------------
-- OrList

newtype OrList a = OrList { fromOrList :: [a] } deriving (Ord, Eq)

instance Functor OrList where
   fmap f (OrList xs) = OrList (map f xs)

instance Monad OrList where
   return = OrList . return
   OrList m >>= f = OrList (m >>= fromOrList . f)

instance Once OrList where
   onceM = useOnceJoin

instance Switch OrList where
   switch (OrList xs) = liftM OrList (sequence xs)

instance OnceJoin OrList where
   onceJoinM f (OrList xs) = 
      liftM OrList (onceJoinM (liftM fromOrList . f) xs)

instance Arbitrary a => Arbitrary (OrList a) where
   arbitrary = do 
      n  <- choose (1, 3)
      xs <- vector n
      return (OrList xs)
   coarbitrary (OrList xs) = coarbitrary xs

instance Show a => Show (OrList a) where
   show (OrList xs) 
      | null xs   = "false"
      | otherwise = unwords (intersperse "or" (map show xs))