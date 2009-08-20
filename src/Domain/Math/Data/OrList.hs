module Domain.Math.Data.OrList where

import qualified Text.OpenMath.Conversion as OM -- Preferably not
import Control.Monad
import Test.QuickCheck
import Data.List (intersperse)

------------------------------------------------------------
-- OrList

newtype OrList a = OrList [a] deriving (Ord, Eq)

instance Functor OrList where
   fmap f (OrList xs) = OrList (map f xs)

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

instance OM.IsOMOBJ a => OM.IsOMOBJ (OrList a) where 
   toOMOBJ (OrList xs) = OM.listop OM.orSymbol xs
   fromOMOBJ =  OM.fromN OM.orSymbol OrList 
             OM.|> (liftM (\x -> OrList [x]) . OM.fromOMOBJ)