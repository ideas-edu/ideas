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
module Domain.LinearAlgebra.Vector 
   ( Vector, VectorSpace
   , makeVectorSpace, vectors, sameDimension, gramSchmidt
   , fromList, toList, liftV, liftV2, showVectorWith
   , toUnit, isUnit, isZero, makeOrthogonal, orthogonal, orthonormalList
   , scale, norm, distance, vectorSum, innerProduct, dimension
   ) where

import Control.Monad
import Common.Rewriting
import Data.List
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative
import Domain.Math.Simplification
import Domain.Math.Expr.Symbols (openMathSymbol) 
import Test.QuickCheck
import qualified Text.OpenMath.Dictionary.Linalg2 as OM

-------------------------------------------------------------------------------
-- Data types

newtype Vector a = V [a]
   deriving (Eq, Ord)
   
newtype VectorSpace a = VS [Vector a]
   deriving (Eq, Ord)

-------------------------------------------------------------------------------
-- Instances

instance Functor Vector where
   fmap f (V xs) = V (map f xs)

instance Foldable Vector where
   foldMap f (V xs) = foldMap f xs

instance Traversable Vector where
   sequenceA (V xs) = V <$> sequenceA xs

instance Show a => Show (Vector a) where
   show = showVectorWith show

instance Num a => Num (Vector a) where
   (+) = liftV2 (+)
   (*) = liftV2 (*)
   (-) = liftV2 (-)
   negate = liftV negate
   abs    = liftV abs
   signum = liftV signum
   fromInteger = fromList . return . fromInteger

instance IsTerm a => IsTerm (Vector a) where
   toTerm = function vectorSymbol . map toTerm . toList
   fromTerm a = do
      xs <- isFunction vectorSymbol a
      ys <- mapM fromTerm xs
      return (fromList ys)

instance Arbitrary a => Arbitrary (Vector a) where
   arbitrary   = liftM fromList $ oneof $ map vector [0..2]

instance CoArbitrary a => CoArbitrary (Vector a) where
   coarbitrary = coarbitrary . toList

vectorSymbol :: Symbol
vectorSymbol = openMathSymbol OM.vectorSymbol

instance Simplify a => Simplify (Vector a) where
   simplifyWith opt = fmap (simplifyWith opt)

instance Functor VectorSpace where
   fmap f (VS xs) = VS (map (fmap f) xs)

instance Show a => Show (VectorSpace a) where
   show = show . vectors

instance IsTerm a => IsTerm (VectorSpace a) where
   toTerm = toTerm . vectors
   fromTerm a = do
      xs <- fromTerm a
      guard (sameDimension xs)
      return (makeVectorSpace xs)
      
instance Simplify a => Simplify (VectorSpace a) where
   simplifyWith opt = fmap (simplifyWith opt)

instance Arbitrary a => Arbitrary (VectorSpace a) where
   arbitrary = do
      i <- choose (0, 3) -- too many vectors "disables" prime factorization
      j <- choose (0, 10 `div` i)
      xs <- replicateM i (liftM fromList $ replicateM j arbitrary)
      return $ makeVectorSpace xs

instance CoArbitrary a => CoArbitrary (VectorSpace a) where
   coarbitrary = coarbitrary . vectors

-------------------------------------------------------------------------------
-- Vector Space operations

-- Check whether all vectors have same dimension
sameDimension :: [Vector a] -> Bool
sameDimension xs =
   case map dimension xs of
      []   -> True
      n:ns -> all (==n) ns

-- | Checks that all vectors in vector space have same dimension
makeVectorSpace :: [Vector a] -> VectorSpace a
makeVectorSpace xs 
   | sameDimension xs = VS xs
   | otherwise        = error "makeVectorSpace: different dimensions" 

vectors :: VectorSpace a -> [Vector a]
vectors (VS xs) = xs

gramSchmidt :: Floating a => VectorSpace a -> VectorSpace a
gramSchmidt (VS xs) = VS (reverse (foldr op [] xs))
 where
   op a as = toUnit (foldr makeOrthogonal a as):as

-------------------------------------------------------------------------------
-- Vector operations

showVectorWith :: (a -> String) -> Vector a -> String
showVectorWith f (V xs) = "(" ++ concat (intersperse "," (map f xs)) ++ ")"

toList :: Vector a -> [a]
toList (V xs) = xs

fromList :: [a] -> Vector a
fromList = V

-- local helper function
liftV :: (a -> b) -> Vector a -> Vector b
liftV op = fromList . map op . toList

-- local helper function
liftV2 :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftV2 op v1 v2 = fromList $ zipWith op (toList v1) (toList v2)

toUnit :: Floating a => Vector a -> Vector a
toUnit v = scale (1 / norm v) v

isUnit :: Floating a => Vector a -> Bool
isUnit v = norm v == 1

isZero :: Num a => Vector a -> Bool
isZero = all (==0) . toList

makeOrthogonal :: Num a => Vector a -> Vector a -> Vector a
makeOrthogonal v1 v2 = v2 - scale (innerProduct v1 v2) v1

orthogonal :: Num a => Vector a -> Vector a -> Bool
orthogonal v1 v2 = innerProduct v1 v2 == 0

scale :: Num a => a -> Vector a -> Vector a
scale a = liftV (*a)

orthonormalList :: Floating a => [Vector a] -> Bool
orthonormalList xs = all isUnit xs && all (uncurry orthogonal) pairs
 where
   pairs = [ (a, b) | (i, a) <- zip [0::Int ..] xs, (j, b) <- zip [0..] xs, i < j ] 

-- length of the vector (also called norm)
norm :: Floating a => Vector a -> a
norm v = sqrt $ innerProduct v v
 
distance :: Floating a => Vector a -> Vector a -> a
distance v1 v2 = norm (v1 - v2)
 
vectorSum :: Num a => Vector a -> a
vectorSum = sum . toList
 
innerProduct :: Num a => Vector a -> Vector a -> a
innerProduct v1 v2 = vectorSum (v1 * v2)

dimension :: Vector a -> Int
dimension = length . toList